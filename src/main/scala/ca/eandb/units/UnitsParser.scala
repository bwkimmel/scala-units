package ca.eandb.units

import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

trait Units {
}

class PrimitiveUnits extends Units

case object OneUnits extends Units

case class RationalScalar(n: BigInt, d: BigInt) extends Units
case class DecimalScalar(value: BigDecimal) extends Units
case class IntegerScalar(value: BigInt) extends Units

case class UnitsRef(symbol: String) extends Units

sealed trait SymbolDef

case class UnitDef(name: String, units: Units) extends SymbolDef
case class PrefixDef(name: String, units: Units) extends SymbolDef

case class QuotientUnits(n: Units, d: Units) extends Units
case class ReciprocalUnits(u: Units) extends Units

case class PowerUnits(base: Units, exp: Int) extends Units

case class ProductUnits(terms: List[Units]) extends Units

object UnitsParsers extends JavaTokenParsers {

  lazy val dimensionless: Parser[Units] =
    "!" ~ "dimensionless" ^^^ { OneUnits }

  lazy val primitive: Parser[Units] =
    "!" ^^ { case _ => new PrimitiveUnits }

  lazy val symbol: Parser[Units] =
    ident ^^ { case symbol => UnitsRef(symbol) }

  lazy val integer: Parser[Units] =
    wholeNumber ^^ { case value => IntegerScalar(BigInt(value)) }

  lazy val decimal: Parser[Units] =
    floatingPointNumber ^^ { case value => DecimalScalar(BigDecimal(value)) }

  lazy val rational: Parser[Units] =
    wholeNumber ~ "|" ~ wholeNumber ^^ {
      case n ~_~ d => RationalScalar(BigInt(n), BigInt(d))
    }

  lazy val scalar: Parser[Units] =
    rational | decimal | integer

  lazy val base: Parser[Units] =
    "(" ~> quotient <~ ")" |
    "(" ~> product <~ ")" |
    symbol |
    scalar

  lazy val power: Parser[Units] =
    base ~ "^" ~ wholeNumber ^^ { case base ~_~ exp => PowerUnits(base, exp.toInt) }

  lazy val product: Parser[Units] =
    term.+ ^^ {
      case term :: Nil => term
      case terms => ProductUnits(terms)
    }

  lazy val quotient: Parser[Units] =
    product ~ "/" ~ product ^^ { case n ~_~ d => QuotientUnits(n, d) }

  lazy val reciprocal: Parser[Units] =
    "/" ~> product ^^ { case u => ReciprocalUnits(u) }

  lazy val term: Parser[Units] =
    dimensionless | primitive | power | base

  lazy val units: Parser[Units] = quotient | reciprocal | product

  lazy val definition: Parser[SymbolDef] =
    ident ~ "-" ~ units ^^ { case name ~_~ units => PrefixDef(name, units) } |
    ident ~ units ^^ { case name ~ units => UnitDef(name, units) }


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

  def definitions(source: Source): Seq[SymbolDef] =
    lines(source).map(parse(definition, _)).flatMap {
      case Success(sdef, _) => Some(sdef)
      case _ => None
    }
 
}

  
  

