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

import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

sealed trait Units extends Ordered[Units] {
  def canonical: CanonicalUnits
  def isScalar: Boolean = false

  def dimensions: Map[PrimitiveUnits, Int] = canonical.dimensions

  def canConvertTo(that: Units): Boolean =
    this.dimensions == that.dimensions

  def convertTo(that: Units): Units = {
    val ratio = QuotientUnits(this, that).canonical
    if (!ratio.isScalar)
      throw new IncompatibleUnitsException(this, that)
    that match {
      case ProductUnits(terms) => ProductUnits(ratio.scale :: terms)
      case u => ProductUnits(List(ratio.scale, that))
    }
  }

  def in(that: Units): Units = this convertTo that

  def *(that: Units): Units = that match {
    case ProductUnits(terms) => ProductUnits(this :: terms)
    case s: Scalar => s * this
    case _ => ProductUnits(List(this, that))
  }

  def /(that: Units): Units = this * that.reciprocal
  def reciprocal: Units

  def pow(n: Int): Units

  def compare(that: Units): Int = {
    val ratio = QuotientUnits(this, that).canonical
    if (!ratio.isScalar)
      throw new IncompatibleUnitsException(this, that)
    ratio.scale.decimalValue compare 1
  }

  def +(that: Units): Units = {
    val a = this.canonical
    val b = that.canonical
    if (a.dimensions != b.dimensions)
      throw new IncompatibleUnitsException(this, that)

    CanonicalUnits(a.scale + b.scale, a.dimensions) in this.root
  }

  def -(that: Units): Units = {
    val a = this.canonical
    val b = that.canonical
    if (a.dimensions != b.dimensions)
      throw new IncompatibleUnitsException(this, that)

    CanonicalUnits(a.scale - b.scale, a.dimensions) in this.root
  }

  def root: Units = this

  def label: String
  def termLabel: String = label
  override def toString = label
}

abstract class NonScalarUnits extends Units {
  def reciprocal: Units = ReciprocalUnits(this)
  def pow(n: Int): Units = PowerUnits(this, n)
}

trait Scalar extends Units {
  override def isScalar: Boolean = true
  def canonicalScalar: Scalar
  def canonical = CanonicalUnits(canonicalScalar)

  override def reciprocal: Scalar
  override def pow(n: Int): Scalar

  def *(that: Scalar): Scalar
  def /(that: Scalar): Scalar = this * that.reciprocal.asInstanceOf[Scalar]

  override def *(that: Units): Units = that match {
    case s: Scalar => this * s
    case CanonicalUnits(scale, dims) =>
      CanonicalUnits(this * scale, dims)
    case ProductUnits((s: Scalar) :: rest) =>
      ProductUnits((this * s) :: rest)
    case _ => super.*(that)
  }

  override def /(that: Units): Units = that match {
    case s: Scalar => this / s
    case _ => super./(that)
  }

  def +(that: Scalar): Scalar
  def -(that: Scalar): Scalar = this + (-that)
  def unary_- : Scalar

  def decimalValue: BigDecimal
}

case class CanonicalUnits(scale: Scalar, override val dimensions: Map[PrimitiveUnits, Int] = Map.empty)
    extends NonScalarUnits {
  require(dimensions.values.forall(_ != 0), "All exponents must be non-zero")

  def canonical = this
  override def isScalar = dimensions.isEmpty

  def expand: Units = {
    val dims = dimensions.toList map {
      case (b, 1) => b
      case (b, e) => PowerUnits(b, e)
    }
    val terms = scale :: dims

    terms.reduce(_ * _)
  }

  def *(that: CanonicalUnits): CanonicalUnits = {
    val s = scale * that.scale
    val dims =
      dimensions.keySet union that.dimensions.keySet flatMap { key =>
        val exp = dimensions.getOrElse(key, 0) + that.dimensions.getOrElse(key, 0)
        if (exp != 0) Some(key -> exp) else None
      } toMap

    CanonicalUnits(s, dims)
  }

  override def *(that: Units): Units = that match {
    case u : CanonicalUnits => this * u
    case s : Scalar => CanonicalUnits(scale * s, dimensions)
    case u => super.*(u)
  }

  override def reciprocal = CanonicalUnits(scale.reciprocal.asInstanceOf[Scalar], dimensions.mapValues(-_))

  def label = expand.label
}

class IncompatibleUnitsException(from: Units, to: Units)
  extends IllegalArgumentException("Incompatible units: %s -> %s".format(from, to))

class UnitsParsingException(units: String)
  extends IllegalArgumentException("Unable to parse units: %s".format(units))

class UnitsDefParsingException(defs: String)
  extends IllegalArgumentException("Unable to parse unit definition: %s".format(defs))

class UndefinedUnitsException(symbol: String)
  extends IllegalArgumentException("Cannot resolve symbol: %s".format(symbol))

case class PrimitiveUnits(symbol: String) extends NonScalarUnits {
  def canonical = CanonicalUnits(OneUnits, Map() + (this -> 1))
  def label = symbol
}

case object OneUnits extends Scalar {
  def canonicalScalar = this
  def +(that: Scalar) = IntegerScalar(1) + that
  def unary_- = IntegerScalar(-1)
  override def *(that: Scalar) = that
  override def /(that: Scalar) = that.reciprocal.asInstanceOf[Scalar]
  override def pow(n: Int) = this
  override def reciprocal = this
  def label = "1"
  def decimalValue = BigDecimal(1)
}

case class RationalScalar(n: BigInt, d: BigInt) extends Scalar {
  def canonicalScalar = if (n == d) OneUnits else {
    val r = n gcd d

    if (r == d)
      IntegerScalar(n / d)
    else if (r == 1)
      this
    else
      RationalScalar(n / r, d / r)
  }

  def +(that: Scalar) = that match {
    case RationalScalar(n2, d2) => RationalScalar(n * d2 + n2 * d, d * d2).canonicalScalar
    case IntegerScalar(n2) => RationalScalar(n + n2 * d, d).canonicalScalar
    case DecimalScalar(x) => DecimalScalar(x + decimalValue).canonicalScalar
    case OneUnits => RationalScalar(n + d, d).canonicalScalar
  }

  def unary_- = RationalScalar(-n, d)

  override def *(that: Scalar) = that match {
    case RationalScalar(n2, d2) => RationalScalar(n * n2, d * d2).canonicalScalar
    case IntegerScalar(n2) => RationalScalar(n * n2, d).canonicalScalar
    case DecimalScalar(x) => DecimalScalar(x * decimalValue).canonicalScalar
    case OneUnits => this
  }

  override def pow(e: Int): Scalar = e match {
    case 0 => OneUnits
    case e if e > 0 => RationalScalar(n pow e, d pow e).canonicalScalar
    case e if e < 0 => RationalScalar(d pow -e, n pow -e).canonicalScalar
  }

  override def reciprocal = RationalScalar(d, n).canonicalScalar
  def label = "%s|%s".format(n, d)
  def decimalValue = BigDecimal(n) / BigDecimal(d)
}

case class DecimalScalar(value: BigDecimal) extends Scalar {
  def canonicalScalar = value.toBigIntExact match {
    case Some(n) => IntegerScalar(n).canonicalScalar
    case None => this
  }

  def +(that: Scalar) =
    DecimalScalar(value + that.decimalValue).canonicalScalar

  def unary_- = DecimalScalar(-value).canonicalScalar

  override def *(that: Scalar) = that match {
    case OneUnits => this
    case _ => DecimalScalar(value * that.decimalValue).canonicalScalar
  }

  override def pow(e: Int): Scalar =
    DecimalScalar(value pow e).canonicalScalar

  override def reciprocal = DecimalScalar(BigDecimal(1) / value)
  def label = value.toString
  def decimalValue = value
}

case class IntegerScalar(value: BigInt) extends Scalar {
  def canonicalScalar = if (value == 1) OneUnits else this
  override def reciprocal = RationalScalar(1, value).canonicalScalar

  def +(that: Scalar) = RationalScalar(value, 1) + that
  def unary_- = IntegerScalar(-value).canonicalScalar

  override def *(that: Scalar) = that match {
    case RationalScalar(n, d) => RationalScalar(value * n, d).canonicalScalar
    case IntegerScalar(n) => IntegerScalar(value * n).canonicalScalar
    case DecimalScalar(x) => DecimalScalar(decimalValue * x).canonicalScalar
    case OneUnits => this
  }

  override def pow(e: Int): Scalar = e match {
    case 0 => OneUnits
    case e if e > 0 => IntegerScalar(value pow e).canonicalScalar
    case e if e < 0 => RationalScalar(1, value pow -e).canonicalScalar
  }

  def label = value.toString
  def decimalValue = BigDecimal(value)
}

sealed abstract case class SymbolDef(name: String, units: Units)

case class UnitDef(override val name: String, override val units: Units) extends SymbolDef(name, units)
case class PrefixDef(override val name: String, override val units: Units) extends SymbolDef(name, units)

case class QuotientUnits(n: Units, d: Units) extends NonScalarUnits {
  def label = "%s / %s".format(n.termLabel, d.termLabel)
  override def termLabel = "(%s)".format(label)
  def canonical =
    ProductUnits(List(n, PowerUnits(d, -1))).canonical

  override def root =
    if (n isScalar)
      ReciprocalUnits(d).root
    else if (d isScalar)
      n root
    else
      QuotientUnits(n root, d root)
}

case class ReciprocalUnits(u: Units) extends NonScalarUnits {
  def label = "/ %s".format(u.termLabel)
  override def termLabel = "(%s)".format(label)
  def canonical =
    PowerUnits(u, -1).canonical
  override def root = if (u isScalar) OneUnits else ReciprocalUnits(u root)
}

case class PowerUnits(base: Units, exp: Int) extends NonScalarUnits {
  def label = base match {
    case b : PowerUnits => "(%s)^%d".format(base.label, exp)
    case _ => "%s^%d".format(base.termLabel, exp)
  }

  def canonical = (base.canonical, exp) match {
    case (_, 0) => OneUnits.canonical
    case (b, 1) => b
    case (CanonicalUnits(scale, dims), e) =>
      CanonicalUnits((scale pow e).asInstanceOf[Scalar], dims.mapValues(_ * e))
  }

  override def pow(n: Int): Units = base match {
    case PowerUnits(b, e) => PowerUnits(b, e * n)
    case _ => super.pow(n)
  }
}

case class ProductUnits(terms: List[Units]) extends NonScalarUnits {
  def label = {
    val result = new StringBuilder
    def build(ts: List[Units]): Unit = ts match {
      case Nil =>
      case (t : QuotientUnits) :: Nil =>
        result.append(t.label)
      case (t : ReciprocalUnits) :: Nil =>
        result.append(t.label)
      case t :: Nil =>
        result.append(t.termLabel)
      case t :: rest =>
        result.append(t.termLabel)
        result.append(" ")
        build(rest)
    }
    build(terms)
    result.toString
  }

  override def termLabel = "(%s)".format(label)

  def canonical = terms.map(_.canonical).reduce(_ * _)

  override def root = terms.map(_.root).filterNot(_.isScalar) match {
    case Nil => OneUnits
    case t :: Nil => t
    case ts => ProductUnits(ts)
  }

  override def *(that: Units): Units = that match {
    case ProductUnits(rterms) => ProductUnits(terms ::: rterms)
    case _ => ProductUnits(terms :+ that)
  }
}

class UnitsParser extends JavaTokenParsers {

  private var _defs: Map[String, SymbolDef] = Map.empty

  case class UnitsRef(symbol: String) extends NonScalarUnits {
    def label = symbol
    def canonical = resolve canonical
  
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
  }

  lazy val dimensionless: Parser[Units] =
    "!" ~ "dimensionless" ^^^ { OneUnits }

  lazy val primitive: Parser[Units] =
    "!" ^^ { case _ => PrimitiveUnits("!") }

  lazy val name1 = """[^+*/\|^;~#()\s_,\.\d-][^+*/\|^;~#()\s-]*""".r
  lazy val name2 = """^(.*[^_,\.1-9])$""".r
  lazy val name3 = """^(.*_[\d\.,]*[1-9])$""".r
 
  lazy val name: Parser[String] =
    name1 ^? {
      case name2(s) => s
      case name3(s) => s
    }

  lazy val nameWithExponent1 = """[^+*/\|^;~#()\s_,\.\d-][^+*/\|^;~#()\s-]*[2-9]""".r
  lazy val nameWithExponent2 = """^(.*[^_,\.1-9])([2-9])$""".r
  lazy val nameWithExponent3 = """^(.*_[\d\.,]*[1-9])([2-9])$""".r

  lazy val nameWithExponent: Parser[Units] =
    nameWithExponent1 ^? {
      case nameWithExponent2(name, exp) => PowerUnits(UnitsRef(name), exp.toInt)
      case nameWithExponent3(name, exp) => PowerUnits(UnitsRef(name), exp.toInt)
    }

  lazy val symbol: Parser[Units] =
    name ^? {
      case symbol if symbol != "per" => UnitsRef(symbol)
    }

  lazy val decimal: Parser[Units] =
    floatingPointNumber ^^ { case value => DecimalScalar(BigDecimal(value)) }

  lazy val rational: Parser[Units] =
    wholeNumber ~ "|" ~ wholeNumber ^^ {
      case n ~_~ d => RationalScalar(BigInt(n), BigInt(d))
    }

  lazy val scalar: Parser[Units] = rational | decimal

  lazy val base: Parser[Units] =
    "(" ~> quotient <~ ")" |
    "(" ~> product <~ ")" |
    symbol |
    scalar

  lazy val power: Parser[Units] =
    nameWithExponent |
    base ~ ("^" ~> wholeNumber).+ ^? {
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

        PowerUnits(base, eval(exps.map(_.toInt)))
    }

  lazy val product: Parser[Units] =
    term.+ ^^ {
      case term :: Nil => term
      case terms => ProductUnits(terms)
    }

  lazy val quotient: Parser[Units] =
    product ~ ("/" | "per") ~ product ^^ { case n ~_~ d => QuotientUnits(n, d) }

  lazy val reciprocal: Parser[Units] =
    ("/" | "per") ~> product ^^ { case u => ReciprocalUnits(u) }

  lazy val term: Parser[Units] =
    dimensionless | primitive | power | base

  lazy val single: Parser[Units] = quotient | reciprocal | product

  lazy val units: Parser[Units] =
    rep1sep(single, "*") ^^ {
      case term :: Nil => term
      case terms => ProductUnits(terms)
    }

  lazy val definition: Parser[SymbolDef] =
    name ~ "-" ~ units ^^ { case name ~_~ units => PrefixDef("%s-" format name, units) } |
    name ~ units ^^ {
      case name ~ PrimitiveUnits(_) => UnitDef(name, PrimitiveUnits(name))
      case name ~ units => UnitDef(name, units)
    }

  def lines(source: Source): Seq[String] = {

    val withContinuation = """^([^#]*)\\$""".r
    val withOptComment = "^([^#]*)(?:#.*)?$".r
    val blank = """^\s*(?:#.*)?$""".r

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

  def create(symbol: String): Units = UnitsRef(symbol)

  def parse(s: String): Units = parseAll(units, s) match {
    case Success(u, _) => u
    case _ => throw new UnitsParsingException(s)
  }

  def apply(s: String): Units = parse(s)

  def load(source: Source) {
    val seed: Map[String, SymbolDef] = Map.empty
    _defs ++= lines(source).map(parseAll(definition, _)).foldLeft(seed) {
      case (defs, Success(sdef, _)) => defs + (sdef.name -> sdef)
      case (defs, _) => defs
    }
  }

  def define(spec: String) {
    val result = parseAll(definition, spec) match {
      case Success(sdef, _) => sdef
      case _ => throw new UnitsDefParsingException(spec)
    }
    _defs += (result.name -> result)
  }

  def convert(from: String, to: String): Units =
    parse(from) convertTo parse(to)
 
}

object Helpers {
  implicit def decimal2units(value: BigDecimal) = DecimalScalar(value)
  implicit def bigInt2units(value: BigInt) = IntegerScalar(value)
  implicit def int2units(value: Int) = IntegerScalar(BigInt(value))
  implicit def long2units(value: Long) = IntegerScalar(BigInt(value))
  implicit def double2units(value: Double) = DecimalScalar(BigDecimal(value))
}

