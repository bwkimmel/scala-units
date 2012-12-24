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

import java.math.MathContext

sealed trait Units extends Ordered[Units] {
  def canonical: CanonicalUnits
  def isScalar: Boolean = false

  def dimensions: Map[PrimitiveUnits, Int] = canonical.dimensions

  def canConvertTo(that: Units): Boolean =
    this.dimensions == that.dimensions

  def convertTo(that: Units): Units = {
    val ratio = (this / that).canonical
    if (!ratio.isScalar)
      throw new IncompatibleUnitsException(this, that)
    that match {
      case CanonicalUnits(scale, dimensions) =>
        CanonicalUnits(scale * ratio.scale, dimensions)
      case ProductUnits(OneUnits :: rest) => ProductUnits(ratio.scale :: rest)
      case ProductUnits(terms) => ProductUnits(ratio.scale :: terms)
      case u => ProductUnits(List(ratio.scale, that))
    }
  }

  def in(that: Units): Units = this convertTo that
  def is(that: Units): Boolean = this canConvertTo that

  def inAllOf(units: Seq[Units]): Seq[Units] = units match {
    case Seq() => Nil
    case Seq(that) => Seq(this convertTo that)
    case Seq(that, rest @ _*) =>
      val (scale, u) = (this convertTo that).split
      scale.truncate match {
        case (intPart, fracPart) =>
          val remainder = if (intPart isZero) this else (fracPart * u)
          (intPart * u) +: (remainder inAllOf rest)
      }
  }

  def inAllOf(first: Units, rest: Units*): Seq[Units] = inAllOf(first +: rest)

  def in(units: Seq[Units]): Seq[Units] = inAllOf(units) filterNot (_.isZero)
  def in(first: Units, rest: Units*): Seq[Units] = in(first +: rest)

  def inOneOf(units: Units*): Units = {
    def scale(u: Units) = u.split._1
    def scan(units: Seq[Units]): Units = units match {
      case Seq(u) => u
      case Seq(u, _*) if scale(u) >= OneUnits => u
      case Seq(_, rest @ _*) => scan(rest)
    }
    scan(units.view.map(this in _))
  }

  def *(that: Units): Units = that match {
    case ProductUnits(terms) => ProductUnits(this :: terms)
    case s: Scalar => s * this
    case _ => ProductUnits(List(this, that))
  }

  def /(that: Units): Units = this * that.reciprocal
  def per(that: Units): Units = this / that
  def reciprocal: Units

  def pow(n: Int): Units
  def **(n: Int): Units = this pow n

  def compare(that: Units): Int = {
    val ratio = (this / that).canonical
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

  def mapScalars(f: Scalar => Scalar): Units = this

  def split: (Scalar, Units) = (OneUnits, this)
  def isZero: Boolean = split._1.isZero

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
  override def split = (this, OneUnits)

  def truncate: (IntegerScalar, Scalar)

  override def mapScalars(f: Scalar => Scalar) = f(this)

  def apply(that: Units) = this * that

  def *(that: Scalar): Scalar
  def /(that: Scalar): Scalar = this * that.reciprocal

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

  override def isZero: Boolean

  def decimalValue: BigDecimal
}

case class CanonicalUnits(scale: Scalar, override val dimensions: Map[PrimitiveUnits, Int] = Map.empty)
    extends NonScalarUnits {
  require(dimensions.values.forall(_ != 0), "All exponents must be non-zero")

  def canonical = this
  override def isScalar = dimensions.isEmpty
  override def root = CanonicalUnits(OneUnits, dimensions)
  override def split = (scale, root)

  override def mapScalars(f: Scalar => Scalar) = CanonicalUnits(f(scale), dimensions)

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

  override def reciprocal = CanonicalUnits(scale.reciprocal, dimensions.mapValues(-_))

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
  def canonical = CanonicalUnits(OneUnits, Map(this -> 1))
  def label = symbol
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

  def truncate = (n /% d) match {
    case (q, r) =>
      (IntegerScalar(q).canonicalScalar,
        RationalScalar(r, d).canonicalScalar)
  }

  def +(that: Scalar) = that match {
    case OneUnits => RationalScalar(n + d, d).canonicalScalar
    case IntegerScalar(n2) => RationalScalar(n + n2 * d, d).canonicalScalar
    case RationalScalar(n2, d2) => RationalScalar(n * d2 + n2 * d, d * d2).canonicalScalar
    case DecimalScalar(x) => DecimalScalar(x + decimalValue).canonicalScalar
  }

  def unary_- = RationalScalar(-n, d)

  override def *(that: Scalar) = that match {
    case OneUnits => this
    case IntegerScalar(n2) => RationalScalar(n * n2, d).canonicalScalar
    case RationalScalar(n2, d2) => RationalScalar(n * n2, d * d2).canonicalScalar
    case DecimalScalar(x) => DecimalScalar(x * decimalValue).canonicalScalar
  }

  override def pow(e: Int): Scalar = e match {
    case 0 => OneUnits
    case e if e > 0 => RationalScalar(n pow e, d pow e).canonicalScalar
    case e if e < 0 => RationalScalar(d pow -e, n pow -e).canonicalScalar
  }

  override def reciprocal = RationalScalar(d, n).canonicalScalar
  def label = "%s|%s".format(n, d)

  override def isZero = (n == 0)
  def decimalValue = BigDecimal(n) / BigDecimal(d)
}

case class DecimalScalar(value: BigDecimal) extends Scalar {
  def canonicalScalar = value.toBigIntExact match {
    case Some(n) => IntegerScalar(n).canonicalScalar
    case None => this
  }

  def truncate = (value /% 1) match {
    case (n, r) =>
      (IntegerScalar(n.toBigInt).canonicalScalar,
        DecimalScalar(r).canonicalScalar)
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

  override def isZero = (value == 0)
  def decimalValue = value
}

case class IntegerScalar(value: BigInt) extends Scalar {
  def canonicalScalar: IntegerScalar = if (value == 1) OneUnits else this
  override def reciprocal = RationalScalar(1, value).canonicalScalar

  def truncate = (this, IntegerScalar(0))

  def +(that: Scalar) = RationalScalar(value, 1) + that
  def unary_- = IntegerScalar(-value).canonicalScalar

  override def *(that: Scalar) = that match {
    case OneUnits => this
    case IntegerScalar(n) => IntegerScalar(value * n).canonicalScalar
    case RationalScalar(n, d) => RationalScalar(value * n, d).canonicalScalar
    case DecimalScalar(x) => DecimalScalar(decimalValue * x).canonicalScalar
  }

  override def pow(e: Int): Scalar = e match {
    case 0 => OneUnits
    case e if e > 0 => IntegerScalar(value pow e).canonicalScalar
    case e if e < 0 => RationalScalar(1, value pow -e).canonicalScalar
  }

  def label = value.toString

  override def isZero = (value == 0)
  def decimalValue = BigDecimal(value)
}

case object OneUnits extends IntegerScalar(1) {
  override def canonicalScalar = this
  override def truncate = (this, IntegerScalar(0))
  override def unary_- = IntegerScalar(-1)
  override def *(that: Scalar) = that
  override def /(that: Scalar) = that.reciprocal
  override def pow(n: Int) = this
  override def reciprocal = this
  override def label = "1"
  override def isZero = false
}

sealed abstract case class SymbolDef(name: String, units: Units)

case class UnitDef(override val name: String, override val units: Units) extends SymbolDef(name, units)
case class PrefixDef(override val name: String, override val units: Units) extends SymbolDef(name, units)

case class ReciprocalUnits(u: Units) extends NonScalarUnits {
  def label = "/ %s".format(u.termLabel)
  override def termLabel = "(%s)".format(label)
  def canonical =
    PowerUnits(u, -1).canonical
  override def root = if (u isScalar) OneUnits else ReciprocalUnits(u root)
  override def split = u.split match {
    case (scale, d) => (scale.reciprocal, d.reciprocal)
  }
  override def reciprocal = u
  override def mapScalars(f: Scalar => Scalar) = ReciprocalUnits(u mapScalars f)
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
      CanonicalUnits(scale pow e, dims.mapValues(_ * e))
  }

  override def pow(n: Int): Units = base match {
    case PowerUnits(b, e) => PowerUnits(b, e * n)
    case _ => super.pow(n)
  }

  override def mapScalars(f: Scalar => Scalar) = PowerUnits(base mapScalars f, exp)
}

case class ProductUnits(terms: List[Units]) extends NonScalarUnits {
  def label = {
    val result = new StringBuilder
    def build(ts: List[Units]): Unit = ts match {
      case Nil =>
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

  override def root = (terms.map(_.root).filterNot(_.isScalar) map {
    case t: PowerUnits => t
    case t => PowerUnits(t, 1)
  } groupBy { _.base } mapValues { _.map(_.exp).sum } flatMap {
    case (b, 0) => None
    case (b, 1) => Some(b)
    case (b, e) => Some(PowerUnits(b, e))
  }).toList match {
    case Nil => OneUnits
    case t :: Nil => t
    case ts => ProductUnits(ts)
  }

  override def split = {
    val parts = terms.map(_.split)
    val scale = parts.map(_._1).reduce(_ * _)

    (scale, ProductUnits(parts.map(_._2)).root)
  }

  override def mapScalars(f: Scalar => Scalar) =
    ProductUnits(terms map (_ mapScalars f))

  override def *(that: Units): Units = that match {
    case ProductUnits(rterms) => ProductUnits(terms ::: rterms)
    case _ => ProductUnits(terms :+ that)
  }
}

class UnitsParser extends JavaTokenParsers {

  private var _defs: Map[String, SymbolDef] = Map.empty

  private case class UnitsRef(symbol: String) extends NonScalarUnits {
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

  private lazy val dimensionless: Parser[Units] =
    "!" ~ "dimensionless" ^^^ { OneUnits }

  private lazy val primitive: Parser[Units] =
    "!" ^^ { case _ => PrimitiveUnits("!") }

  private lazy val name1 = """[^+*/\|^;~#()\s_,\.\d-][^+*/\|^;~#()\s-]*""".r
  private lazy val name2 = """^(.*[^_,\.1-9])$""".r
  private lazy val name3 = """^(.*_[\d\.,]*[1-9])$""".r
 
  private lazy val name: Parser[String] =
    name1 ^? {
      case name2(s) => s
      case name3(s) => s
    }

  private lazy val nameWithExponent1 = """[^+*/\|^;~#()\s_,\.\d-][^+*/\|^;~#()\s-]*[2-9]""".r
  private lazy val nameWithExponent2 = """^(.*[^_,\.1-9])([2-9])$""".r
  private lazy val nameWithExponent3 = """^(.*_[\d\.,]*[1-9])([2-9])$""".r

  private lazy val nameWithExponent: Parser[Units] =
    nameWithExponent1 ^? {
      case nameWithExponent2(name, exp) => PowerUnits(UnitsRef(name), exp.toInt)
      case nameWithExponent3(name, exp) => PowerUnits(UnitsRef(name), exp.toInt)
    }

  private lazy val symbol: Parser[Units] =
    name ^? {
      case symbol if symbol != "per" => UnitsRef(symbol)
    }

  private lazy val decimal: Parser[Units] =
    floatingPointNumber ^^ { case value => DecimalScalar(BigDecimal(value)) }

  private lazy val rational: Parser[Units] =
    wholeNumber ~ "|" ~ wholeNumber ^^ {
      case n ~_~ d => RationalScalar(BigInt(n), BigInt(d))
    }

  private lazy val scalar: Parser[Units] = rational | decimal

  private lazy val base: Parser[Units] =
    "(" ~> quotient <~ ")" |
    "(" ~> product <~ ")" |
    symbol |
    scalar

  private lazy val power: Parser[Units] =
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

  private lazy val product: Parser[Units] =
    term.+ ^^ {
      case term :: Nil => term
      case terms => ProductUnits(terms)
    }

  private lazy val quotient: Parser[Units] =
    product ~ ("/" | "per") ~ product ^^ { case n ~_~ d => n / d }

  private lazy val reciprocal: Parser[Units] =
    ("/" | "per") ~> product ^^ { case u => ReciprocalUnits(u) }

  private lazy val term: Parser[Units] =
    dimensionless | primitive | power | base

  private lazy val single: Parser[Units] = quotient | reciprocal | product

  private lazy val units: Parser[Units] =
    rep1sep(single, "*") ^^ {
      case term :: Nil => term
      case terms => ProductUnits(terms)
    }

  private lazy val definition: Parser[SymbolDef] =
    name ~ "-" ~ units ^^ { case name ~_~ units => PrefixDef("%s-" format name, units) } |
    name ~ units ^^ {
      case name ~ PrimitiveUnits(_) => UnitDef(name, PrimitiveUnits(name))
      case name ~ units => UnitDef(name, units)
    }

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
  implicit def decimal2units(value: BigDecimal) = DecimalScalar(value).canonicalScalar
  implicit def bigInt2units(value: BigInt) = IntegerScalar(value).canonicalScalar
  implicit def int2units(value: Int) = IntegerScalar(BigInt(value)).canonicalScalar
  implicit def long2units(value: Long) = IntegerScalar(BigInt(value)).canonicalScalar
  implicit def double2units(value: Double) = DecimalScalar(BigDecimal(value)).canonicalScalar

  trait HelperMethods {
    def withMaxDenominator(maxd: BigInt): Units
    def withNoRationals: Units
    def withMathContext(mc: MathContext): Units
    def withPrecision(precision: Int): Units
    def withScale(scale: Int): Units
  }

  implicit def units2helpers(u: Units): HelperMethods = new HelperMethods {
    def withMaxDenominator(maxd: BigInt): Units = u mapScalars Helpers.withMaxDenominator(maxd)
    def withNoRationals: Units = u mapScalars Helpers.withNoRationals
    def withMathContext(mc: MathContext): Units = u mapScalars Helpers.withMathContext(mc)
    def withPrecision(precision: Int): Units = u mapScalars Helpers.withPrecision(precision)
    def withScale(scale: Int): Units = u mapScalars Helpers.withScale(scale)
  }

  def withMaxDenominator(maxd: BigInt)(x: Scalar): Scalar = x match {
    case RationalScalar(_, d) if d > maxd =>
      DecimalScalar(x.decimalValue).canonicalScalar
    case _ => x
  }

  def withNoRationals(x: Scalar): Scalar = x match {
    case _: RationalScalar => DecimalScalar(x.decimalValue).canonicalScalar
    case _ => x
  }

  def withMathContext(mc: MathContext)(x: Scalar): Scalar = x match {
    case DecimalScalar(value) => DecimalScalar(value(mc)).canonicalScalar
    case IntegerScalar(value) => DecimalScalar(BigDecimal(value)(mc)).canonicalScalar
    case _ => x
  }

  def withPrecision(precision: Int): Scalar => Scalar =
    withMathContext(new MathContext(precision))

  def withScale(scale: Int)(x: Scalar): Scalar = x match {
    case DecimalScalar(value) =>
      val mode = BigDecimal.RoundingMode.HALF_UP
      DecimalScalar(value setScale (scale, mode)).canonicalScalar
    case IntegerScalar(value) if scale < 0 =>
      val mode = BigDecimal.RoundingMode.HALF_UP
      IntegerScalar(BigDecimal(value) setScale (scale, mode) toBigInt).canonicalScalar
    case _ => x
  }
}

