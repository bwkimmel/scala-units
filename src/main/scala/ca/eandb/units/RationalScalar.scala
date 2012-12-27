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

/**
 * A Scalar that is the ratio of two integers.
 * @param n The numerator
 * @param d The denominator (must be positive)
 */
class RationalScalar(val n: BigInt, val d: BigInt) extends Scalar {
  require(d > 0, "denominator must be positive")

  def canonicalScalar: RationalScalar = if (n == d) OneUnits else {
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
    case _ => DecimalScalar(decimalValue + that.decimalValue).canonicalScalar
  }

  override def unary_- = RationalScalar(-n, d)

  def *(that: RationalScalar): RationalScalar = that match {
    case OneUnits => this
    case IntegerScalar(n2) => RationalScalar(n * n2, d).canonicalScalar
    case RationalScalar(n2, d2) => RationalScalar(n * n2, d * d2).canonicalScalar
  }

  override def *(that: Scalar) = that match {
    case ExactScalar(r, ds) => ExactScalar(this * r, ds)
    case _: RationalScalar => this * that
    case _ => ExactScalar(this, that.decimalValue -> 1)
  }

  override def pow(e: Int): RationalScalar =
    if (e >= 0)
      RationalScalar(n pow e, d pow e).canonicalScalar
    else
      reciprocal pow -e

  override def reciprocal: RationalScalar =
    if (n > 0)
      RationalScalar(d, n).canonicalScalar
    else
      RationalScalar(-d, -n).canonicalScalar

  def label = "%s|%s".format(n, d)

  override def isZero = (n == 0)
  def decimalValue = BigDecimal(n) / BigDecimal(d)
}

object RationalScalar {

  def apply(n: BigInt, d: BigInt): RationalScalar = new RationalScalar(n, d)

  def unapply(s: Scalar): Option[(BigInt, BigInt)] = s match {
    case r: RationalScalar => Some(r.n -> r.d)
    case _ => None
  }

}
