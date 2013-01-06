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

/** Represents a quantity with associated units. */
trait Units extends Ordered[Units] {

  /** <code>CanonicalUnits</code> equivalent to this instance. */
  def canonical: CanonicalUnits

//  import OpMatchers._
//  def normalize: Units = this match {
//    case (a * b) * c => a * (b * c) normalize
//    case (a / b) * c => (a * c) / b normalize
//    case (a / b) / c => a / (b * c) normalize
//    case a * (b / c) => (a * b) / c normalize
//    case (a @ PrimitiveUnits(x)) * ((b @ PrimitiveUnits(y)) * c) if x > y => b * (a * c) normalize
//    case a ~ n if n < 0 => OneUnits / a ~ -n normalize
//    case OneUnits * b => b normalize
//    case a * OneUnits => a normalize
//    case (a: Scalar) * (b: Scalar) => a * b
//    case (a: Scalar) / (b: Scalar) => a / b
//    case a * (b: Scalar) => b * a
//    case a / (b: Scalar) => b.reciprocal * a
//  }

  /** Determines if these Units are dimensionless (scalar). */
  def isScalar: Boolean = false

  /** Gets these Units as a Scalar value, if they are Scalar. */
  def asScalarOption: Option[Scalar] = {
    val u = canonical
    if (u.isScalar) Some(u.scale) else None
  }

  /** Gets these Units as a Scalar value. */
  def asScalar = asScalarOption.get

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
   *   90 (seconds) inOneOf Seq(hours, minutes, seconds)
   *   => 1.5 minutes
   *
   *   2100 (MHz) inOneOf Seq(GHz, MHz, kHz, Hz)
   *   => 2.1 GHz
   * </pre>
   *
   * @param units The sequence of Units to try to convert these Units to.  The
   *   units should be provided in descending order by scale.
   * @throws IncompatibleUnitsException if these Units cannot be converted to
   *   one of the provided sequence of Units.
   */
  def inOneOf(units: Seq[Units]): Units = {
    def scale(u: Units) = u.split._1
    def scan(units: Seq[Units]): Units = units match {
      case Seq(u) => u
      case Seq(u, _*) if scale(u) >= OneUnits => u
      case Seq(_, rest @ _*) => scan(rest)
    }
    scan(units.view.map(this in _))
  }

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
   * @param first The first of the sequence of Units to try to convert these
   *   units to.
   * @param rest The rest of the sequence of Units to try to convert these units
   *   to.
   * @throws IncompatibleUnitsException if these Units cannot be converted to
   *   one of the provided sequence of Units.
   */

  def inOneOf(first: Units, rest: Units*): Units = inOneOf(first +: rest)

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
   * @param n The exponent (must be more than zero) 
   */
  protected[units] def pow(n: Int): Units

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
   * Negates these Units.
   * @return The additive inverse of <code>this</code>.
   */
  def unary_- : Units = IntegerScalar(-1) * this

  /**
   * Adds these Units without resolving them.
   * @param that The Units to add
   * @return Units representing the abstract sum of these units and
   *   <code>that</code>.  Note that incompatible units will not be detected
   *   until the units are resolved.
   */
  def ++(that: Units): Units = (this, that) match {
    case (SumUnits(left), SumUnits(right)) => SumUnits(left ::: right)
    case (SumUnits(left), right) => SumUnits(left :+ right)
    case (left, SumUnits(right)) => SumUnits(left :: right)
    case (left, right) => SumUnits(left :: right :: Nil)
  }

  /**
   * Subtracts the specified Units without resolving them.
   * @param that The Units to subtract
   * @return Units representing the abstract difference of these units and
   *   <code>that</code>.  Note that incompatible units will not be detected
   *   until the units are resolved.
   */
  def --(that: Units): Units = (this, that) match {
    case (SumUnits(left), SumUnits(right)) => SumUnits(left ::: right.map(-_))
    case (SumUnits(left), right) => SumUnits(left :+ -right)
    case (left, SumUnits(right)) => SumUnits(left :: right.map(-_))
    case (left, right) => SumUnits(left :: -right :: Nil)
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

