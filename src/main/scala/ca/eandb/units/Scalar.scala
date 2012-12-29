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

/** Represents dimensionless quantities */
trait Scalar extends Units {

  override def isScalar: Boolean = true
  def canonical = CanonicalUnits(canonicalScalar)

  /** Expresses this Scalar in canonical form. */
  def canonicalScalar: Scalar

  override def split = (this, OneUnits)
  override def reciprocal: Scalar
  override protected[units] def pow(n: Int): Scalar

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

  /** The additive inverse of this Scalar. */
  override def unary_- : Scalar = sys.error("Not implemented")

  /**
   * Subtracts another Scalar from this
   * @param that The Scalar to subtract
   */
  def -(that: Scalar): Scalar = this + (-that)

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

