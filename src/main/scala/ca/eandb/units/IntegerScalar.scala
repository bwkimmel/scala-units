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

/** An integer-valued Scalar. */
case class IntegerScalar(value: BigInt) extends RationalScalar(value, 1) {
  override def canonicalScalar: IntegerScalar = if (value == 1) OneUnits else this
  override def reciprocal =
    RationalScalar(value.signum, value.abs).canonicalScalar

  override def truncate = (this, IntegerScalar(0))

  override def unary_- = IntegerScalar(-value).canonicalScalar

  override def *(that: Scalar) = that match {
    case OneUnits => this
    case IntegerScalar(n) => IntegerScalar(value * n).canonicalScalar
    case RationalScalar(n, d) => RationalScalar(value * n, d).canonicalScalar
    case DecimalScalar(x) => ExactScalar(this, x -> 1)
    case e: ExactScalar => e * this
  }

  override def pow(e: Int): RationalScalar =
    if (e >= 0)
      IntegerScalar(value pow e).canonicalScalar
    else
      reciprocal pow -e

  override def label = value.toString

  override def isZero = (value == 0)
  override def decimalValue = BigDecimal(value)
}

