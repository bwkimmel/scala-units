package ca.eandb.units

import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source


sealed trait Units {
  def canonical: Units = this
  def isScalar: Boolean = false
  def dimensions = canonical match {
    case _: Scalar => OneUnits
    case ProductUnits(terms) =>
      ProductUnits(terms.filterNot(_.isScalar)).canonical
    case u => u
  }

  def canConvertTo(that: Units): Boolean =
    this.dimensions == that.dimensions

  def convertTo(that: Units): Units = {
    val ratio = QuotientUnits(this, that).canonical
    if (!ratio.dimensions.isScalar)
      throw new IncompatibleUnitsException(this, that)
    ratio * that
  }

  def in(that: Units): Units = this convertTo that

  def *(that: Units): Units = that match {
    case ProductUnits(terms) => ProductUnits(this :: terms)
    case _ => ProductUnits(List(this, that))
  }

  def /(that: Units): Units = this * reciprocal
  def reciprocal: Units = ReciprocalUnits(this)

  def pow(n: Int): Units = PowerUnits(this, n)

  def label: String
  def termLabel: String = label
  override def toString = label
}

class IncompatibleUnitsException(from: Units, to: Units)
  extends IllegalArgumentException("Incompatible units: %s -> %s".format(from, to))

class UnitsParsingException(units: String)
  extends IllegalArgumentException("Unable to parse units: %s".format(units))

class UnitsDefParsingException(defs: String)
  extends IllegalArgumentException("Unable to parse unit definition: %s".format(defs))

class UndefinedUnitsException(symbol: String)
  extends IllegalArgumentException("Cannot resolve symbol: %s".format(symbol))

case class PrimitiveUnits(symbol: String) extends Units {
  def label = symbol
}

trait Scalar extends Units {
  override def isScalar: Boolean = true
}

case object OneUnits extends Scalar {
  override def *(that: Units) = that
  override def /(that: Units) = that reciprocal
  override def reciprocal = this
  def label = "1"
}

case class RationalScalar(n: BigInt, d: BigInt) extends Scalar {
  override def canonical = if (n == d) OneUnits else {
    val r = n gcd d

    if (r == 1)
      this
    else if (r == d)
      IntegerScalar(n / d)
    else
      RationalScalar(n / r, d / r)
  }

  override def *(that: Units) = that match {
    case RationalScalar(n2, d2) => RationalScalar(n * n2, d * d2)
    case IntegerScalar(n2) => RationalScalar(n * n2, d)
    case DecimalScalar(x) => DecimalScalar(x * (BigDecimal(n) / BigDecimal(d)))
    case _ => super.*(that)
  }

  override def reciprocal = RationalScalar(d, n)
  def label = "%s|%s".format(n, d)
}

case class DecimalScalar(value: BigDecimal) extends Scalar {
  override def canonical = value.toBigIntExact match {
    case Some(n) => IntegerScalar(n).canonical
    case None => this
  }

  override def *(that: Units) = that match {
    case RationalScalar(n, d) => DecimalScalar(value * (BigDecimal(n) / BigDecimal(d)))
    case IntegerScalar(n) => DecimalScalar(value * BigDecimal(n))
    case DecimalScalar(x) => DecimalScalar(value * x)
    case _ => super.*(that)
  }

  override def reciprocal = DecimalScalar(BigDecimal(1) / value)
  def label = value.toString
}

case class IntegerScalar(value: BigInt) extends Scalar {
  override def canonical = if (value == 1) OneUnits else this
  override def reciprocal = RationalScalar(1, value)

  override def *(that: Units) = that match {
    case RationalScalar(n, d) => RationalScalar(value * n, d)
    case IntegerScalar(n) => IntegerScalar(value * n)
    case DecimalScalar(x) => DecimalScalar(BigDecimal(value) * x)
    case _ => super.*(that)
  }
  def label = value.toString
}

case class UnitsRef(symbol: String, defs: String => Option[SymbolDef]) extends Units {
  def label = symbol
  override def canonical = resolve canonical

  private def resolve: Units = {

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

sealed abstract case class SymbolDef(name: String, units: Units)

case class UnitDef(override val name: String, override val units: Units) extends SymbolDef(name, units)
case class PrefixDef(override val name: String, override val units: Units) extends SymbolDef(name, units)

case class QuotientUnits(n: Units, d: Units) extends Units {
  def label = "%s / %s".format(n.termLabel, d.termLabel)
  override def termLabel = "(%s)".format(label)
  override def canonical =
    ProductUnits(List(n, PowerUnits(d, -1))).canonical
}

case class ReciprocalUnits(u: Units) extends Units {
  def label = "/ %s".format(u.termLabel)
  override def termLabel = "(%s)".format(label)
  override def canonical =
    PowerUnits(u, -1).canonical
}

case class PowerUnits(base: Units, exp: Int) extends Units {
  def label = base match {
    case b : PowerUnits => "(%s)^%d".format(base.label, exp)
    case _ => "%s^%d".format(base.termLabel, exp)
  }

  override def canonical = (base.canonical, exp) match {
    case (b, 0) => OneUnits
    case (b, 1) => b
    case (ProductUnits(terms), e) => ProductUnits(terms.map(PowerUnits(_, e))).canonical
    case (PowerUnits(b, e1), e2) => PowerUnits(b, e1 * e2).canonical
    case (DecimalScalar(x), e) => DecimalScalar(x pow e).canonical
    case (RationalScalar(n, d), e) if e > 0 => RationalScalar(n pow e, d pow e).canonical
    case (RationalScalar(n, d), e) if e < 0 => RationalScalar(d pow -e, n pow -e).canonical
    case (IntegerScalar(x), e) if e > 0 => IntegerScalar(x pow e).canonical
    case (IntegerScalar(x), e) if e < 0 => RationalScalar(1, x pow -e).canonical
    case (b, e) => PowerUnits(b, e)
  }
  override def pow(n: Int): Units = base match {
    case PowerUnits(b, e) => PowerUnits(b, e * n)
    case _ => super.pow(n)
  }
}

case class ProductUnits(terms: List[Units]) extends Units {
  def label = terms.map(_.termLabel).mkString(" ")
  override def termLabel = "(%s)".format(label)

  private def flatten(terms: List[Units]): List[Units] = terms.map(_.canonical) flatMap {
    case ProductUnits(ts) => flatten(ts)
    case OneUnits => Nil
    case term => term :: Nil
  }

  private def key(terms: Units) = terms match {
    case PowerUnits(PrimitiveUnits(symbol), _) => Some(symbol)
    case PrimitiveUnits(symbol) => Some(symbol)
    case _ => None
  }

  override def canonical = flatten(terms) match {
    case Nil => OneUnits
    case term :: Nil => term
    case terms =>
      val seed: Units = OneUnits
      val scalar = terms.collect { case x: Scalar => x }.foldLeft(seed) {
        case (a, b) => a * b
      }.canonical

      val dims = terms.groupBy(key).toList.sortBy(_._1) flatMap {
        case (Some(symbol), ts) =>
          val u = PrimitiveUnits(symbol)
          ts.map {
            case PowerUnits(_, e) => e
            case _ => 1
          }.sum match {
            case 0 => None
            case 1 => Some(u)
            case e => Some(PowerUnits(u, e))
          }
        case _ => None
      }
      
      val all = scalar match {
        case OneUnits => dims
        case s => s :: dims
      }
      
      flatten(all) match {
        case Nil => OneUnits
        case term :: Nil => term
        case terms => ProductUnits(terms)
      }
  }

  override def *(that: Units): Units = that match {
    case ProductUnits(rterms) => ProductUnits(terms ::: rterms)
    case _ => ProductUnits(terms :+ that)
  }
}

class UnitsParser extends JavaTokenParsers {

  private var _defs: Map[String, SymbolDef] = Map.empty
  def resolve(s: String): Option[SymbolDef] = _defs.get(s)

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
      case nameWithExponent2(name, exp) => PowerUnits(UnitsRef(name, resolve), exp.toInt)
      case nameWithExponent3(name, exp) => PowerUnits(UnitsRef(name, resolve), exp.toInt)
    }

  lazy val symbol: Parser[Units] =
    name ^? {
      case symbol if symbol != "per" => UnitsRef(symbol, resolve)
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
    product ~ "per" ~ product ^^ { case n ~_~ d => QuotientUnits(n, d) } |
    product ~ "/" ~ product ^^ { case n ~_~ d => QuotientUnits(n, d) }

  lazy val reciprocal: Parser[Units] =
    "/" ~> product ^^ { case u => ReciprocalUnits(u) }

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

  def create(symbol: String): Units = UnitsRef(symbol, resolve)

  def parse(s: String): Units = parseAll(units, s) match {
    case Success(u, _) => u
    case _ => throw new UnitsParsingException(s)
  }

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

