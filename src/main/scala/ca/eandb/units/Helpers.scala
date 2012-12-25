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

import java.math.MathContext

/** Helper methods for working with units. */
object Helpers {

  // Implicit conversions for scalars
  implicit def decimal2units(value: BigDecimal) = DecimalScalar(value).canonicalScalar
  implicit def bigInt2units(value: BigInt) = IntegerScalar(value).canonicalScalar
  implicit def int2units(value: Int) = IntegerScalar(BigInt(value)).canonicalScalar
  implicit def long2units(value: Long) = IntegerScalar(BigInt(value)).canonicalScalar
  implicit def double2units(value: Double) = DecimalScalar(BigDecimal(value)).canonicalScalar

  // Helper methods for transforming scalar parts of units
  trait HelperMethods {
    def withMaxDenominator(maxd: BigInt): Units
    def withNoRationals: Units
    def withMathContext(mc: MathContext): Units
    def withPrecision(precision: Int): Units
    def withScale(scale: Int): Units
  }

  // Implicit application of HelperMethods
  implicit def units2helpers(u: Units): HelperMethods = new HelperMethods {
    def withMaxDenominator(maxd: BigInt): Units = u mapScalars Helpers.withMaxDenominator(maxd)
    def withNoRationals: Units = u mapScalars Helpers.withNoRationals
    def withMathContext(mc: MathContext): Units = u mapScalars Helpers.withMathContext(mc)
    def withPrecision(precision: Int): Units = u mapScalars Helpers.withPrecision(precision)
    def withScale(scale: Int): Units = u mapScalars Helpers.withScale(scale)
  }

  // Scalar transformation methods

  /**
   * Transforms rational scalars into decimal scalars if the denominator is
   * more than a certain value.
   * @param maxd The maximum denominator to accept.  If the scalar is a
   *   rational with its denominator at most <code>maxd</code>, it will be left
   *   as a rational.
   * @param x The scalar value to transform.
   * @return A scalar that, if rational, is guaranteed to have a denominator no
   *   greater than <code>maxd</code>, and who's value is equivalent to
   *   <code>x</code> within the precision of the default MathContext.
   */
  def withMaxDenominator(maxd: BigInt)(x: Scalar): Scalar = x match {
    case RationalScalar(_, d) if d > maxd =>
      DecimalScalar(x.decimalValue).canonicalScalar
    case _ => x
  }

  /**
   * Transforms rational scalars into decimal scalars.
   * @param x The scalar value to transform.
   * @return A non-rational scalar who's value is equivalent to
   *   <code>x</code> within the precision of the default MathContext.
   */
  def withNoRationals(x: Scalar): Scalar = x match {
    case _: RationalScalar => DecimalScalar(x.decimalValue).canonicalScalar
    case _ => x
  }

  /**
   * Applies the specified MathContext to decimal and integer scalars.
   * @param mc The MathContext to apply
   * @param x The scalar to transform
   * @return If <code>x</code> is integral or decimal, the specified
   *   MathContext (<code>mc</code>) will be applied.  If <code>x</code> is
   *   rational, no transformation is applied.
   */
  def withMathContext(mc: MathContext)(x: Scalar): Scalar = x match {
    case DecimalScalar(value) => DecimalScalar(value(mc)).canonicalScalar
    case IntegerScalar(value) => DecimalScalar(BigDecimal(value)(mc)).canonicalScalar
    case _ => x
  }

  /**
   * Rounds an integral or decimal scalar to the specified precision (number of
   * significant digits).
   * @param precision The number of significant digits.  The digits are counted
   *   from the most significant digit.  For example:
   *   <ul>
   *     <li><code>128.withPrecision(2) == 130</code></li>
   *     <li><code>3.14159.withPrecision(4) == 3.142</code></li>
   *   </ul>
   * @param x The scalar to transform
   * @return If <code>x</code> is integral or decimal, it will be rounded to the
   *   specified precision.  If <code>x</code> is rational, no transformation is
   *   applied.
   */
  def withPrecision(precision: Int): Scalar => Scalar =
    withMathContext(new MathContext(precision))

  /**
   * Rounds an integral or decimal scalar to the specified scale (number of
   * decimal places).
   * @param scale The number of decimal places.  The digits are counted from
   *   the decimal point.  May be negative.
   *   Examples:
   *   <ul>
   *     <li><code>3.14159.withScale(2) == 3.14</code></li>
   *     <li><code>128.withScale(-1) == 130</code></li>
   *   </ul>
   * @param x The scalar to transform
   * @return If <code>x</code> is integral or decimal, it will be rounded to the
   *   specified scale.  If <code>x</code> is rational, no transformation is
   *   applied.
   */
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

