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

  override def unary_- = DecimalScalar(-value).canonicalScalar

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

