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

/** Represents a quantity with associated units. */
sealed trait Units extends Ordered[Units] {

  /** <code>CanonicalUnits</code> equivalent to this instance. */
  def canonical: CanonicalUnits

  /** Determines if these Units are dimensionless (scalar). */
  def isScalar: Boolean = false

  /** The dimensions of these Units (i.e., powers of the primitive units). */
  def dimensions: Map[PrimitiveUnits, Int] = canonical.dimensions

  /**
   * Determines if we can convert to the specified Units.
   * @param that The Units to test for compatibility with.
   */
  def canConvertTo(that: Units): Boolean =
    this.dimensions == that.dimensions

  /**
   * Converts these Units to the specified Units
   * @param that The Units to convert to
   * @throws IncompatibleUnitsException if the dimensions of these Units are
   *   different from the dimensions of <code>that</code>.
   */
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

  /**
   * Converts these Units to the specified Units
   * @param that The Units to convert to
   * @throws IncompatibleUnitsException if the dimensions of these Units are
   *   different from the dimensions of <code>that</code>.
   */
  def in(that: Units): Units = this convertTo that

  /**
   * Determines if we can convert to the specified Units.
   * @param that The Units to test for compatibility with.
   */
  def is(that: Units): Boolean = this canConvertTo that

  /**
   * Converts these Units to the provided sequence of units.  All units
   * returned will be an integral multiple of the provided units except
   * potentially the last.
   * 
   * Example:
   * <pre>
   *   90.5 (seconds) inAllOf Seq(hours, minutes, seconds)
   *   => (0 hours, 1 minute, 30.5 seconds)
   * </pre>
   *
   * @param units The sequence of Units to convert to.  These should be in
   *   descending order by scale.
   * @throws IncompatibleUnitsException if these Units cannot be converted to
   *   one of the provided sequence of Units.
   */
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

  /**
   * Converts these Units to the provided sequence of units.  All units
   * returned will be an integral multiple of the provided units except
   * potentially the last.
   * 
   * Example:
   * <pre>
   *   90.5 (seconds) inAllOf (hours, minutes, seconds)
   *   => (0 hours, 1 minute, 30.5 seconds)
   * </pre>
   *
   * @param first The first of the sequence of Units to convert to.
   * @param rest The rest of the sequence of Units to convert to.
   * @throws IncompatibleUnitsException if these Units cannot be converted to
   *   one of the provided sequence of Units.
   */
  def inAllOf(first: Units, rest: Units*): Seq[Units] = inAllOf(first +: rest)

  /**
   * Converts these Units to the provided sequence of units.  All units
   * returned will be an integral multiple of the provided units except
   * potentially the last.  Zero-valued units are removed.
   * 
   * Examples:
   * <pre>
   *   90.5 (seconds) in Seq(hours, minutes, seconds)
   *   => (1 minute, 30.5 seconds)
   *
   *   3602 (seconds) in Seq(hours, minutes, seconds)
   *   => (1 hour, 2 seconds)
   * </pre>
   *
   * @param units The sequence of Units to convert to.  These should be in
   *   descending order by scale.
   * @throws IncompatibleUnitsException if these Units cannot be converted to
   *   one of the provided sequence of Units.
   */
  def in(units: Seq[Units]): Seq[Units] = inAllOf(units) filterNot (_.isZero)

  /**
   * Converts these Units to the provided sequence of units.  All units
   * returned will be an integral multiple of the provided units except
   * potentially the last.  Zero-valued units are removed.
   * 
   * Examples:
   * <pre>
   *   90.5 (seconds) in (hours, minutes, seconds)
   *   => (1 minute, 30.5 seconds)
   *
   *   3602 (seconds) in (hours, minutes, seconds)
   *   => (1 hour, 2 seconds)
   * </pre>
   *
   * @param first The first of the sequence of Units to convert to.
   * @param rest The rest of the sequence of Units to convert to.
   * @throws IncompatibleUnitsException if these Units cannot be converted to
   *   one of the provided sequence of Units.
   */
  def in(first: Units, rest: Units*): Seq[Units] = in(first +: rest)

  /**
   * Converts these Units to one of the provided sequence of Units.  The Units
   * that are selected will be the first in the sequence with a scalar value of
   * at least one.  If the scalar value in all provided Units is less than one,
   * the return value will be expressed in the last units.
   *
   * Examples:
   * <pre>
   *   90 (seconds) inOneOf (hours, minutes, seconds)
   *   => 1.5 minutes
   *
   *   2100 (MHz) inOneOf (GHz, MHz, kHz, Hz)
   *   => 2.1 GHz
   * </pre>
   *
   * @param units The sequence of Units to try to convert these Units to.  The
   *   units should be provided in descending order by scale.
   * @throws IncompatibleUnitsException if these Units cannot be converted to
   *   one of the provided sequence of Units.
   */
  def inOneOf(units: Units*): Units = {
    def scale(u: Units) = u.split._1
    def scan(units: Seq[Units]): Units = units match {
      case Seq(u) => u
      case Seq(u, _*) if scale(u) >= OneUnits => u
      case Seq(_, rest @ _*) => scan(rest)
    }
    scan(units.view.map(this in _))
  }

  /**
   * Multiplies these Units with another.
   * @param that The Units to multiply by.
   * @return The product of these Units with <code>that</code>.
   */
  def *(that: Units): Units = that match {
    case ProductUnits(terms) => ProductUnits(this :: terms)
    case s: Scalar => s * this
    case _ => ProductUnits(List(this, that))
  }

  /**
   * Divides these Units by another.
   * @param that The Units to divide by.
   * @return The quotient of these Units with <code>that</code>.
   */
  def /(that: Units): Units = this * that.reciprocal

  /**
   * Divides these Units by another.
   * @param that The Units to divide by.
   * @return The quotient of these Units with <code>that</code>.
   */
  def per(that: Units): Units = this / that

  /** Gets the multiplicative inverse of these Units. */
  def reciprocal: Units

  /**
   * Raises these Units to the specified power.
   * @param n The exponent.
   */
  def pow(n: Int): Units

  /**
   * Raises these Units to the specified power.
   * @param n The exponent.
   */
  def ~(n: Int): Units = n match {
    case 0 => OneUnits
    case 1 => this
    case -1 => reciprocal
    case n if n < 0 => reciprocal pow -n
    case n => this pow n
  }

  /**
   * Compares these Units to another.
   * @param that The Units to compare against.
   * @return A value indicating the relative scale of these Units with
   *   <code>that</code>:
   *   <ul>
   *     <li>-1 if <code>this &lt; that</code>.</li>
   *     <li>1 if <code>this &gt; that</code>.</li>
   *     <li>0 if <code>this == that</code>.</li>
   *   </ul>
   * @throws IncompatibleUnitsException if these Units cannot be converted to
   *   <code>that</code>.
   */
  def compare(that: Units): Int = {
    val ratio = (this / that).canonical
    if (!ratio.isScalar)
      throw new IncompatibleUnitsException(this, that)
    ratio.scale.decimalValue compare 1
  }

  /**
   * Adds these Units to another.
   * @param that The Units to add.
   * @return The sum of these Units with <code>that</code>, expressed in the
   *   same units as <code>this</code>.
   * @throws IncompatibleUnitsException if these Units cannot be converted to
   *   <code>that</code>.
   */
  def +(that: Units): Units = {
    val a = this.canonical
    val b = that.canonical
    if (a.dimensions != b.dimensions)
      throw new IncompatibleUnitsException(this, that)

    CanonicalUnits(a.scale + b.scale, a.dimensions) in this.root
  }

  /**
   * Subtracts the specified Units from <code>this</code>.
   * @param that The Units to subtract.
   * @return The difference of these Units with <code>that</code>, expressed in
   *   the same units as <code>this</code>.
   * @throws IncompatibleUnitsException if these Units cannot be converted to
   *   <code>that</code>.
   */
  def -(that: Units): Units = {
    val a = this.canonical
    val b = that.canonical
    if (a.dimensions != b.dimensions)
      throw new IncompatibleUnitsException(this, that)

    CanonicalUnits(a.scale - b.scale, a.dimensions) in this.root
  }

  /**
   * Transforms all of the scalars in these Units.
   * @param f The function to use to transform the scalars.
   * @return These Units with the scalars transformed by <code>f</code>.
   */
  def mapScalars(f: Scalar => Scalar): Units = this

  /**
   * Separates the scalar portion of these Units from the symbolic portion.
   * @return A Scalar <code>x</code> and a Units <code>u</code> not consisting
   *   of any Scalars such that <code>x * u</code> is equivalent to
   *   <code>this</code>.
   */
  def split: (Scalar, Units) = (OneUnits, this)

  /** Indicates if these Units have zero value. */
  def isZero: Boolean = split._1.isZero

  def root: Units = this

  /** A string parsable into Units equivalent to <code>this</code>. */
  def label: String

  /**
   * A string parsable into Units equivalent to <code>this</code> that can be
   * safely combined other unit labels without operator precedence changing
   * these Units' meaning.
   */
  def termLabel: String = label

  /** A string parsable into Units equivalent to <code>this</code>. */
  override def toString = label

}

/** Represents dimensionless quantities */
trait Scalar extends Units {

  override def isScalar: Boolean = true
  def canonical = CanonicalUnits(canonicalScalar)

  /** Expresses this Scalar in canonical form. */
  def canonicalScalar: Scalar

  override def split = (this, OneUnits)
  override def reciprocal: Scalar
  override def pow(n: Int): Scalar

  /**
   * Raises this Scalar to the specified power.
   * @param n The exponent.
   */
  override def ~(n: Int): Scalar = n match {
    case 0 => OneUnits
    case 1 => this
    case -1 => reciprocal
    case n if n < 0 => reciprocal pow -n
    case n => this pow n
  }

  /** Splits this Scalar into integral and fractional parts. */
  def truncate: (IntegerScalar, Scalar)

  override def mapScalars(f: Scalar => Scalar) = f(this)

  /**
   * Applies the specified Units to this Scalar quantity.  Equivalent to
   * <code>this * that</code>.  This is primarily for syntactial convenience.
   * Useful when combined with implicit conversions from primitive types.
   *
   * Example usage:
   * <pre>
   *   val km = units("km")
   *   val x = 50 (km)
   * </pre>
   *
   * @param that The Units to apply.
   */
  def apply(that: Units) = this * that

  /**
   * Multiplies this Scalar with another.
   * @param that The Scalar to multiply by
   */
  def *(that: Scalar): Scalar

  /**
   * Divides this Scalar by another.
   * @param that The Scalar to divide by
   */
  def /(that: Scalar): Scalar = this * that.reciprocal

  /**
   * Adds this Scalar to another.
   * @param that The Scalar to add
   */
  def +(that: Scalar): Scalar

  /**
   * Subtracts another Scalar from this
   * @param that The Scalar to subtract
   */
  def -(that: Scalar): Scalar = this + (-that)

  /** The additive inverse of this Scalar. */
  def unary_- : Scalar

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

  override def isZero: Boolean

  /** The decimal value of this Scalar. */
  def decimalValue: BigDecimal

}

/**
 * Represents units in a form that can be readily compared for compatibility,
 * and which allows direct conversion between instances.
 */
case class CanonicalUnits(scale: Scalar, override val dimensions: Map[PrimitiveUnits, Int] = Map.empty)
    extends Units {

  // Require that no dimensions have zero exponents, so that we can test
  // compatibility by comparing these Maps directly.
  require(dimensions.values.forall(_ != 0), "All exponents must be non-zero")

  def canonical = this
  override def isScalar = dimensions.isEmpty
  override def root = CanonicalUnits(OneUnits, dimensions)
  override def split = (scale, root)

  override def mapScalars(f: Scalar => Scalar) = CanonicalUnits(f(scale), dimensions)

  /** Expresses these Units as products of powers of primitive Units. */
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

  def reciprocal = CanonicalUnits(scale.reciprocal, dimensions.mapValues(-_))
  def pow(n: Int) = CanonicalUnits(scale pow n, dimensions.mapValues(_ * n))

  def label = expand.label
}

/**
 * Indicates an attempt to convert to Units of differing dimensions.
 * @param from The Units that were being converted.
 * @param to The Units to which conversion was requested.
 */
class IncompatibleUnitsException(from: Units, to: Units)
  extends IllegalArgumentException("Incompatible units: %s -> %s".format(from, to))

/**
 * Indicates a parsing error ocurred while attempting to parse units.
 * @param units The string being parsed.
 */
class UnitsParsingException(units: String)
  extends IllegalArgumentException("Unable to parse units: %s".format(units))

/**
 * Indicates a parsing error ocurred while attempting to parse a unit definition.
 * @param defs The string being parsed.
 */
class UnitsDefParsingException(defs: String)
  extends IllegalArgumentException("Unable to parse unit definition: %s".format(defs))

/**
 * Indicates the presence of an undefined unit while attempting to resolve
 * units.
 * @param symbol The symbol for the undefined unit.
 */
class UndefinedUnitsException(symbol: String)
  extends IllegalArgumentException("Cannot resolve symbol: %s".format(symbol))

/**
 * Represents fundamental units (i.e., units which cannot be further reduced).
 * @param symbol The symbol for these Units
 */
case class PrimitiveUnits(symbol: String) extends Units {
  def canonical = CanonicalUnits(OneUnits, Map(this -> 1))
  def label = symbol
  def reciprocal = ReciprocalUnits(this)
  def pow(n: Int) = PowerUnits(this, n)
}

/**
 * A Scalar that is the ratio of two integers.
 * @param n The numerator
 * @param d The denominator (must be positive)
 */
case class RationalScalar(n: BigInt, d: BigInt) extends Scalar {
  require(d > 0, "denominator must be positive")

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

  override def pow(e: Int): Scalar =
    if (e >= 0)
      RationalScalar(n pow e, d pow e).canonicalScalar
    else
      reciprocal pow -e

  override def reciprocal =
    if (n > 0)
      RationalScalar(d, n).canonicalScalar
    else
      RationalScalar(-d, -n).canonicalScalar

  def label = "%s|%s".format(n, d)

  override def isZero = (n == 0)
  def decimalValue = BigDecimal(n) / BigDecimal(d)
}

/** A decimal-valued Scalar. */
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

/** An integer-valued Scalar. */
case class IntegerScalar(value: BigInt) extends Scalar {
  def canonicalScalar: IntegerScalar = if (value == 1) OneUnits else this
  override def reciprocal =
    RationalScalar(value.signum, value.abs).canonicalScalar

  def truncate = (this, IntegerScalar(0))

  def +(that: Scalar) = RationalScalar(value, 1) + that
  def unary_- = IntegerScalar(-value).canonicalScalar

  override def *(that: Scalar) = that match {
    case OneUnits => this
    case IntegerScalar(n) => IntegerScalar(value * n).canonicalScalar
    case RationalScalar(n, d) => RationalScalar(value * n, d).canonicalScalar
    case DecimalScalar(x) => DecimalScalar(decimalValue * x).canonicalScalar
  }

  override def pow(e: Int): Scalar =
    if (e >= 0)
      IntegerScalar(value pow e).canonicalScalar
    else
      reciprocal pow -e

  def label = value.toString

  override def isZero = (value == 0)
  def decimalValue = BigDecimal(value)
}

/** The multiplicative identity. */
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

/**
 * The definition of a unit of prefix.
 * @param name The name of the symbol begin defined
 * @param units The definition of the symbol
 */
sealed abstract case class SymbolDef(name: String, units: Units)

/** Represents a Units definition. */
case class UnitDef(override val name: String, override val units: Units) extends SymbolDef(name, units)

/**
 * Represents a prefix definition (may be combined with other Units to form new
 * Units.
 */
case class PrefixDef(override val name: String, override val units: Units) extends SymbolDef(name, units)

/**
 * The multiplicative inverse of another Units.
 * @param u The reciprocal of these Units
 */
case class ReciprocalUnits(u: Units) extends Units {
  def label = "/ %s".format(u.termLabel)
  override def termLabel = "(%s)".format(label)
  def canonical =
    PowerUnits(u, -1).canonical
  override def root = if (u isScalar) OneUnits else ReciprocalUnits(u root)
  override def split = u.split match {
    case (scale, d) => (scale.reciprocal, d.reciprocal)
  }
  override def mapScalars(f: Scalar => Scalar) = ReciprocalUnits(u mapScalars f)
  def reciprocal = u
  def pow(n: Int) = ReciprocalUnits(u pow n)
}

/**
 * Units raised to an exponent.
 * @param base The base Units
 * @param exp The exponent
 */
case class PowerUnits(base: Units, exp: Int) extends Units {
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

  def reciprocal = ReciprocalUnits(this)
  def pow(n: Int): Units = base match {
    case PowerUnits(b, e) => PowerUnits(b, e * exp * n)
    case _ => PowerUnits(base, exp * n)
  }

  override def mapScalars(f: Scalar => Scalar) = PowerUnits(base mapScalars f, exp)
}

/**
 * The product of a set of Units.
 * @param terms The list of Units being multiplied
 */
case class ProductUnits(terms: List[Units]) extends Units {
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

  def pow(n: Int) = PowerUnits(this, n)
  def reciprocal = ReciprocalUnits(this)
}

/** Parses units. */
class UnitsParser extends JavaTokenParsers {

  /** Unit and prefix definitions. */
  private var _defs: Map[String, SymbolDef] = Map.empty

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
 
  private lazy val name: Parser[String] =
    name1 ^? {
      case name2(s) => s
      case name3(s) => s
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
        DecimalScalar(BigDecimal(n) / BigDecimal(d))
    }

  private lazy val scalar: Parser[Units] = rational | decimal

  private lazy val atom: Parser[Units] =
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

  private lazy val plus: Parser[(Units, Units) => Units] = "+" ^^^ { _ + _ }
  private lazy val minus: Parser[(Units, Units) => Units] = "-" ^^^ { _ - _ }
  private lazy val times: Parser[(Units, Units) => Units] = "*" ^^^ { _ * _ }
  private lazy val divide: Parser[(Units, Units) => Units] = ("/" | "per") ^^^ { _ / _ }

  private lazy val term: Parser[Units] =
    ("/" | "per") ~> term ^^ { case a => a.reciprocal } |
    chainl1(factor, times | divide)

  private lazy val units: Parser[Units] = chainl1(term, plus | minus)

  private lazy val dimensionless: Parser[Units] =
    "!" ~ "dimensionless" ^^^ { OneUnits }

  private lazy val primitive: Parser[Units] =
    "!" ^^ { case _ => PrimitiveUnits("!") }

  private lazy val rhs: Parser[Units] = dimensionless | primitive | units

  private lazy val definition: Parser[SymbolDef] =
    name ~ "-" ~ rhs ^^ { case name ~_~ units => PrefixDef("%s-" format name, units) } |
    name ~ rhs ^^ {
      case name ~ PrimitiveUnits(_) => UnitDef(name, PrimitiveUnits(name))
      case name ~ units => UnitDef(name, units)
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
   *   Addition and subtraction are not supported.
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
   *   Addition and subtraction are not supported.
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
  def load(source: Source) {
    val seed: Map[String, SymbolDef] = Map.empty
    _defs ++= lines(source).map(parseAll(definition, _)).foldLeft(seed) {
      case (defs, Success(sdef, _)) => defs + (sdef.name -> sdef)
      case (defs, _) => defs
    }
  }

  /**
   * Defines a new unit or prefix.  No attempt is made to resolve the unit
   * definition to powers of primitive units.
   *
   * Example usage: <pre>this.define("cent 1|100 dollar")</pre>
   *
   * @param spec The unit definition expression.  Most valid GNU unit definition
   *   expressions are supported.  Non-linear expressions (function calls) are
   *   not supported.  Addition and subtraction are not supported.
   * @throws UnitsDefParsingException if the provided unit definition expression
   *   is invalid or not supported.
   * @see
   *   <a href="http://www.gnu.org/software/units/manual/units.html#Unit-Definitions">
   *     GNU Units - Unit Definitions
   *   </a>
   */
  def define(spec: String) {
    val result = parseAll(definition, spec) match {
      case Success(sdef, _) => sdef
      case _ => throw new UnitsDefParsingException(spec)
    }
    _defs += (result.name -> result)
  }

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

