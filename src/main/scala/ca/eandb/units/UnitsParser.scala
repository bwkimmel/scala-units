/**
 * Scala Units
 * Copyright (C) 2012 Bradley W. Kimmel
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */
package ca.eandb.units

import scala.collection.mutable
import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

import java.util.Locale

/** Parses units. */
class UnitsParser(locale: Locale = Locale.getDefault) extends JavaTokenParsers {

  /** Unit and prefix definitions. */
  val _defs: mutable.Map[String, SymbolDef] = mutable.Map()

  /**
   * An unresolved reference to derived units.
   * @param symbol The symbol representing these Units
   */
  private case class UnitsRef(symbol: String) extends Units {
    def label = symbol
    def canonical = resolve canonical
  
    /** Expands these Units once according to the Units' definition. */
    private def resolve: Units = {
      lazy val defs = _defs.lift
  
      def splits(s: String): Seq[(String, String)] =
        for (i <- 0 to s.length) yield (s splitAt i)
  
      def resolveSplit(prefix: String, name: String) = (defs("%s-" format prefix), defs(name)) match {
        case (Some(PrefixDef(_, scale)), Some(SymbolDef(_, base))) =>
          Some(ProductUnits(List(scale, base)))
        case _ => None
      }
  
      val pluralType1 = "^(.*)s$".r
      val pluralType2 = "^(.*)es$".r
      val pluralType3 = "^(.*)ies$".r
  
      type RootFunc = PartialFunction[String, String]
      val roots: Stream[String] = Stream(
        { case root => root } : RootFunc,
        { case pluralType1(root) => root } : RootFunc,
        { case pluralType2(root) => root } : RootFunc,
        { case pluralType3(root) => root + "y" } : RootFunc
      ) map { _ lift } flatMap { _(symbol) }
  
      roots flatMap splits flatMap {
        case ("", name) => defs(name).map(_.units)
        case (prefix, "") => defs("%s-" format prefix).map(_.units)
        case (prefix, name) => resolveSplit(prefix, name)
      } headOption match {
        case Some(u) => u.canonical
        case None => throw new UndefinedUnitsException(symbol)
      }
    }

    def reciprocal = ReciprocalUnits(this)
    def pow(n: Int) = PowerUnits(this, n)
  }

  //----------------------------------------------------------------------------
  // Parser definitions

  private lazy val name1 = """[^!+*/\|^;~#()\s_,\.\d-][^!+*/\|^;~#()\s-]*""".r
  private lazy val name2 = """^(.*[^_,\.1-9])$""".r
  private lazy val name3 = """^(.*_[\d\.,]*[1-9])$""".r
  private lazy val name4 = """^(.*\d{2,})$""".r
 
  private lazy val name: Parser[String] =
    name1 ^? {
      case name2(s) => s
      case name3(s) => s
      case name4(s) => s
    }

  private lazy val nameWithExponent1 = """[^!+*/\|^;~#()\s_,\.\d-][^!+*/\|^;~#()\s-]*[2-9]""".r
  private lazy val nameWithExponent2 = """^(.*[^_,\.1-9])([2-9])$""".r
  private lazy val nameWithExponent3 = """^(.*_[\d\.,]*[1-9])([2-9])$""".r

  private lazy val nameWithExponent: Parser[Units] =
    nameWithExponent1 ^? {
      case nameWithExponent2(name, exp) => PowerUnits(UnitsRef(name), exp.toInt)
      case nameWithExponent3(name, exp) => PowerUnits(UnitsRef(name), exp.toInt)
    }

  private lazy val symbol: Parser[Units] =
    name ^? { case symbol if symbol != "per" => UnitsRef(symbol) }

  val integer = """(-?\d+)""".r
  private lazy val decimal: Parser[Units] =
    floatingPointNumber ^^ {
      case integer(value) => IntegerScalar(BigInt(value))
      case value => DecimalScalar(BigDecimal(value))
    }

  private lazy val rational: Parser[Units] =
    floatingPointNumber ~ "|" ~ floatingPointNumber ^^ {
      case integer(n) ~_~ integer(d) =>
        val (numer, denom) = (BigInt(n), BigInt(d))
        val sign = denom.signum
        RationalScalar(sign * numer, sign * denom)
      case n ~_~ d =>
        DecimalScalar(BigDecimal(n)) / DecimalScalar(BigDecimal(d))
    }

  private lazy val scalar: Parser[Units] = rational | decimal

  private lazy val function: Parser[Units] =
    ScalarFunction.builtIns
      .map { case f => f.name ~ "(" ~> units <~ ")" ^^ f }
      .reduce { _ | _ }

  private lazy val atom: Parser[Units] =
    function |
    "(" ~> units <~ ")" |
    symbol |
    nameWithExponent |
    scalar

  private lazy val power: Parser[Units] =
    atom ~ (("^" | "**") ~> wholeNumber).+ ^? {
      case base ~ exps if exps.tail.map(_.toInt).forall(_ >= 0) =>
        def pow(b: Int, e: Int, acc: Int = 1): Int = (b, e) match {
          case (_, 0) => acc
          case (0, _) => 0
          case _ => pow(b, e - 1, b * acc)
        }

        def eval(exps: List[Int]): Int = exps match {
          case Nil => 1
          case b :: rest => pow(b, eval(rest))
        }

        base ~ eval(exps.map(_.toInt))
    }

  private lazy val molecule: Parser[Units] = power | atom

  private lazy val factor: Parser[Units] =
    molecule ~ factor ^^ { case a ~ b => a * b } |
    molecule

  private lazy val plus: Parser[(Units, Units) => Units] = "+" ^^^ { _ ++ _ }
  private lazy val minus: Parser[(Units, Units) => Units] = "-" ^^^ { _ -- _ }
  private lazy val times: Parser[(Units, Units) => Units] = "*" ^^^ { _ * _ }
  private lazy val divide: Parser[(Units, Units) => Units] = ("/" | "per") ^^^ { _ / _ }

  private lazy val term: Parser[Units] =
    ("/" | "per") ~> term ^^ { case a => a.reciprocal } |
    chainl1(factor, times | divide)

  private lazy val units: Parser[Units] = chainl1(term, plus | minus)

  //----------------------------------------------------------------------------
  // Command parser definitions

  private lazy val dimensionless: Parser[Units] =
    "!" ~ "dimensionless" ^^^ OneUnits

  private lazy val primitive: Parser[Units] =
    "!" ^^^ PrimitiveUnits("!")

  private lazy val rhs: Parser[Units] = dimensionless | primitive | units

  private lazy val definition: Parser[SymbolDef] =
    name ~ "-" ~ rhs ^^ { case name ~_~ units => PrefixDef("%s-" format name, units) } |
    name ~ rhs ^^ {
      case name ~ PrimitiveUnits(_) => UnitDef(name, PrimitiveUnits(name))
      case name ~ units => UnitDef(name, units)
    }


  /**
   * Represents the current state of the interpreter
   *
   * @param blocks The stack of nested blocks.  Each block indicates the type of
   *   block (so that we can verify that the corresponding "!end" statements
   *   match correctly) and a boolean value denoting whether the block is
   *   enabled (i.e., united definitions and other statements should be
   *   processed).
   * @param vars The current variable definitions (as indicated by "!set"
   *   statements).
   */
  private case class State(blocks: List[(String, Boolean)] = Nil, vars: Map[String, String] = Map()) {

    /**
     * Starts a new block.
     * @param block The type of block (corresponding calls to "end" must have a
     *   matching block type).
     * @param en A value indicating whether the statements within this block
     *   should be processed.
     */
    def start(block: String, en: Boolean) = copy(blocks = (block, en && enabled) :: blocks)

    /**
     * Ends the current block.
     * @param block The type of block to end (must match the corresponding call
     *   to "start").
     * @throws IllegalStateException if <code>block</code> does not match the
     *   type from the corresponding call to start, or if there is no
     *   corresponding call to start.
     */
    def end(block: String) = blocks match {
      case (inBlock, _) :: outer if block == inBlock => copy(blocks = outer)
      case (inBlock, _) :: _ =>
        throw new IllegalStateException(
          "End block (%s) does not match start (%s)".format(block, inBlock))
      case Nil =>
        throw new IllegalStateException(
          "End block (%s) with no matching start".format(block))
    }

    /** Indicates whether statements should be processed in the current block */
    def enabled = blocks match {
      case Nil => true
      case (_, en) :: _ => en
    }

    /** Sets the value of a variable. */
    def set(key: String, value: String) =
      if (enabled) copy(vars = vars + (key -> value)) else this

    /** Prints a message */
    def message(msg: String) { if (enabled) println(msg) }

    /** Defines a new unit or prefix */
    def define(d: SymbolDef) { if (enabled) _defs.put(d.name, d) }

  }

  private lazy val command: Parser[State => State] = {
    // Helpers
    def s(f: State => State): State => State = f
    def action[T](f: State => T): State => State = (s: State) => { f(s); s }
    val any: Parser[String] = ".*".r
    val nop: State => State = (s: State) => s
    val word: Parser[String] = "\\w+".r

    "!" ~ "utf8" ^^^ s(_.start("utf8", false)) |
    "!" ~ "endutf8" ^^^ s(_.end("utf8")) |
    "!" ~ "locale" ~> ident ^^ {
      case id => s(_.start("locale", locale.toString startsWith id)) } |
    "!" ~ "endlocale" ~ any ^^^ s(_.end("locale")) |
    "!" ~ "set" ~> ident ~ any ^^ {
      case key ~ value => s(_.set(key, value)) } |
    "!" ~ "var" ~> ident ~ word.+ ^^ {
      case key ~ values => s(s => s.start("var", values intersect s.vars.get(key).toSeq nonEmpty)) } |
    "!" ~ "varnot" ~> ident ~ word.+ ^^ {
      case key ~ values => s(s => s.start("var", values intersect s.vars.get(key).toSeq isEmpty)) } |
    "!" ~ "endvar" ~ any ^^^ s(_.end("var")) |
    "!" ~ "include" ~ any ^^^ nop |
    "!" ~ "unitlist" ~ any ^^^ nop |
    "!" ~ "message" ~> any ^^ {
      case msg => action(_.message(msg)) } |
    name ~ "(" ~ any ^^^ nop |
    name ~ "[" ~ any ^^^ nop |
    definition ^^ {
      case d => action(_.define(d)) }
  }

  /**
   * Strips comments and processes line continuations.
   * @param source The Source to read from
   * @return A stream of non-empty strings without comments or line
   *   continuations.
   */
  private def lines(source: Source): Seq[String] = {
    val withContinuation = """^([^#]*)\\$""".r
    val withOptComment = "^([^#]*)(?:#.*)?$".r

    def group(lines: Stream[String], prefix: List[String] = Nil): Stream[String] =
      (prefix, lines) match {
        case (Nil, Stream.Empty) => Stream.Empty
        case (_, Stream.Empty) => sys.error("Line continuation at end of file")
        case (_, withContinuation(line) #:: rest) => group(rest, line :: prefix)
        case (_, withOptComment(line) #:: rest) =>
          (line :: prefix).reverse.mkString.trim match {
            case "" => group(rest)
            case full => full #:: group(rest)
          }
      }

    group(source.getLines.toStream)
  }

  /**
   * Creates an unresolved reference to the specified symbol.  If the symbol is
   * undefined, no exception will be thrown until an attempt is made to resolve
   * the reference (for example by canonicalization, or by converting to another
   * Units).
   * @param symbol The symbol to create a reference to.
   */
  def create(symbol: String): Units = UnitsRef(symbol)

  /**
   * Parses the specified units.  No attempt is made to resolve the provided
   * units.
   * @param expr The units expression.  Most valid GNU unit expressions are
   *   supported.  Non-linear expressions (function calls) are not supported.
   * @throws UnitsParsingException if the provided unit expression is invalid or
   *   not supported.
   * @see
   *   <a href="http://www.gnu.org/software/units/manual/units.html#Unit-Expressions">
   *     GNU Units - Unit Expressions
   *   </a>
   */
  def parse(expr: String): Units = parseAll(units, expr) match {
    case Success(u, _) => u
    case _ => throw new UnitsParsingException(expr)
  }

  /**
   * Parses the specified units.  No attempt is made to resolve the provided
   * units.  The <code>apply</code> method is provided for syntactical
   * convenience.
   *
   * Example usage:
   * <pre>
   *   val u = new UnitsParser
   *   // ... load units ...
   *   val km = u("km")
   * </pre>
   *
   * @param expr The units expression.  Most valid GNU unit expressions are
   *   supported.  Non-linear expressions (function calls) are not supported.
   * @throws UnitsParsingException if the provided unit expression is invalid or
   *   not supported.
   * @see
   *   <a href="http://www.gnu.org/software/units/manual/units.html#Unit-Expressions">
   *     GNU Units - Unit Expressions
   *   </a>
   */
  def apply(s: String): Units = parse(s)

  /**
   * Loads unit definitions from the provided Source.  Note that
   * <code>!include</code> directives will not be followed.
   * @param source The Source to read unit definitions from
   */
  def load(source: Source) { (State() /: lines(source))(exec) }

  /**
   * Loads unit definitions from the provided Source.  Note that
   * <code>!include</code> directives will not be followed.
   * @param source The Source to read unit definitions from
   * @param handler The catch block to use to handle parsing exceptions
   */
  def tryLoad(source: Source)(handler: PartialFunction[Throwable, Unit]) {
    (State() /: lines(source))(tryExec(handler))
  }

  /**
   * Defines a new unit or prefix.  No attempt is made to resolve the unit
   * definition to powers of primitive units.
   *
   * Example usage: <pre>this.define("cent 1|100 dollar")</pre>
   *
   * @param spec The unit definition expression.  Most valid GNU unit definition
   *   expressions are supported.  Non-linear expressions (function calls) are
   *   not supported.
   * @throws CommandParsingException if the provided unit definition expression
   *   is invalid or not supported.
   * @see
   *   <a href="http://www.gnu.org/software/units/manual/units.html#Unit-Definitions">
   *     GNU Units - Unit Definitions
   *   </a>
   */
  def define(spec: String) {
    val result = parseAll(definition, spec) match {
      case Success(sdef, _) => sdef
      case _ => throw new CommandParsingException(spec)
    }
    _defs += (result.name -> result)
  }

  /**
   * Executes the provided GNU Units command.
   *
   * Example usage: <pre>this.exec("!set INCH_UNIT canada")</pre>
   *
   * @param state The initial state of the interpreter
   * @param cmd The command to execute
   * @return The state of the interpreter after processing the command
   * @throws CommandParsingException if the command is a unit definition that
   *   is invalid or not supported.
   * @see
   *   <a href="http://www.gnu.org/software/units/manual/units.html#Database-Syntax">
   *     GNU Units - Database Command Syntax
   *   </a>
   */
  private def exec(state: State, cmd: String): State = parseAll(command, cmd) match {
    case Success(f, _) => f(state)
    case e => throw new CommandParsingException(cmd)
  }

  /**
   * Executes the provided GNU Units command.
   *
   * Example usage:
   * <pre>
   *   this.tryExec("!set INCH_UNIT canada") {
   *     case e => println("ERROR: %s".format(e))
   *   }
   * </pre>
   *
   * @param handler The catch block to handle exceptions thrown by the parser
   * @param state The initial state of the interpreter
   * @param cmd The command to execute
   * @return The state of the interpreter after processing the command
   * @throws CommandParsingException if the command is a unit definition that
   *   is invalid or not supported.
   * @see
   *   <a href="http://www.gnu.org/software/units/manual/units.html#Database-Syntax">
   *     GNU Units - Database Command Syntax
   *   </a>
   */
  private def tryExec(handler: PartialFunction[Throwable, Unit])(state: State, cmd: String): State =
    try { exec(state, cmd) } catch (handler andThen ((x: Unit) => state))

  /**
   * Converts one set of units to another.  Equivalent to
   *   <code>parse(from) in parse(to)</code>
   * @param from The string denoting the units to convert
   * @param to The string denoting the units to convert to
   * @returns The value of <code>from</code> expressed in the units
   *   <code>to</code>.
   * @throws IncompatibleUnitsException If <code>from</code> and <code>to</code>
   *   have different dimensions
   * @throws UnitsParsingException If <code>from</code> or <code>to</code>
   *   cannot be parsed.
   * @throws UndefinedUnitsException If <code>from</code> or <code>to</code>,
   *   directly or indirecty, refers to units which cannot be resolved to powers
   *   of primitive units.
   */
  def convert(from: String, to: String): Units =
    parse(from) convertTo parse(to)
 
}

