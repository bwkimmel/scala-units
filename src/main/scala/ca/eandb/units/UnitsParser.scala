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
      throw new IllegalArgumentException("Incompatible units: %s -> %s".format(this, that))
    ratio * that
  }

  def *(that: Units): Units = ProductUnits(List(this, that))
  def /(that: Units): Units = this * reciprocal
  def reciprocal: Units = ReciprocalUnits(this)
}

case class PrimitiveUnits(symbol: String) extends Units

trait Scalar extends Units {
  override def isScalar: Boolean = true
}

case object OneUnits extends Scalar {
  override def *(that: Units) = that
  override def /(that: Units) = that reciprocal
  override def reciprocal = this
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
    case _ => ProductUnits(List(this, that))
  }

  override def reciprocal = RationalScalar(d, n)
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
    case _ => ProductUnits(List(this, that))
  }

  override def reciprocal = DecimalScalar(BigDecimal(1) / value)
}

case class IntegerScalar(value: BigInt) extends Scalar {
  override def canonical = if (value == 1) OneUnits else this
  override def reciprocal = RationalScalar(1, value)

  override def *(that: Units) = that match {
    case RationalScalar(n, d) => RationalScalar(value * n, d)
    case IntegerScalar(n) => IntegerScalar(value * n)
    case DecimalScalar(x) => DecimalScalar(BigDecimal(value) * x)
    case _ => ProductUnits(List(this, that))
  }
}

case class UnitsRef(symbol: String, defs: String => Option[SymbolDef]) extends Units {
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

    val x = roots flatMap splits
    
    val y: Seq[Units] = x flatMap {
      case ("", name) => defs(name).map(_.units)
      case (prefix, "") => defs("%s-" format prefix).map(_.units)
      case (prefix, name) => resolveSplit(prefix, name)
    }

    y.head.canonical
  }
}

sealed abstract case class SymbolDef(name: String, units: Units)

case class UnitDef(override val name: String, override val units: Units) extends SymbolDef(name, units)
case class PrefixDef(override val name: String, override val units: Units) extends SymbolDef(name, units)

case class QuotientUnits(n: Units, d: Units) extends Units {
  override def canonical =
    ProductUnits(List(n, PowerUnits(d, -1))).canonical
}

case class ReciprocalUnits(u: Units) extends Units {
  override def canonical =
    PowerUnits(u, -1).canonical
}

case class PowerUnits(base: Units, exp: Int) extends Units {
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
}

case class ProductUnits(terms: List[Units]) extends Units {
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
    base ~ "^" ~ wholeNumber ^^ { case base ~_~ exp => PowerUnits(base, exp.toInt) }

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

  lazy val units: Parser[Units] = quotient | reciprocal | product

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
    case _ => throw new IllegalArgumentException("Invalid units: %s".format(s))
  }

  def load(source: Source) {
    val seed: Map[String, SymbolDef] = Map.empty
    _defs ++= lines(source).map(parseAll(definition, _)).foldLeft(seed) {
      case (defs, Success(sdef, _)) => defs + (sdef.name -> sdef)
      case (defs, _) => defs
    }
  }
 
}

  
  

