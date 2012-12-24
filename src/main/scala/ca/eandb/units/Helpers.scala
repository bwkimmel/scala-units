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

