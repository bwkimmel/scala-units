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
 * Represents products/quotients of scalar quantities (integer, rational,
 * decimals) without loss of precision.
 *
 * @param rational The rational factor of the scalar quantity
 * @param decimals A Map of the decimal factors of the scalar quantities and
 *   their corresponding exponents.
 */
case class ExactScalar(rational: RationalScalar, decimals: Map[BigDecimal, Int]) extends Scalar {
  require(decimals forall { case (_, e) => e != 0 }, "exponents must be non-zero")

  def label = canonicalScalar label

  /** Expresses this Scalar in canonical form. */
  lazy val canonicalScalar: Scalar =
    if (decimals isEmpty)
      rational.canonicalScalar
    else rational match {
      case RationalScalar(n, d) =>
        val one = BigDecimal(1)
        val values = decimals map { case (b, e) => b pow e } 
        DecimalScalar((BigDecimal(n) / BigDecimal(d)) *
          (one /: values)(_ * _)).canonicalScalar
    }
      
  override def reciprocal: Scalar =
    ExactScalar(rational.reciprocal, decimals.mapValues(-_))

  override def pow(e: Int): Scalar =
    ExactScalar(rational pow e, decimals.mapValues(_ * e))

  /** Splits this Scalar into integral and fractional parts. */
  def truncate: (IntegerScalar, Scalar) = canonicalScalar truncate

  private def combine(f: (Int, Int) => Int)
    (d1: Map[BigDecimal, Int], d2: Map[BigDecimal, Int]): Map[BigDecimal, Int] = {

    val keys = d1.keySet union d2.keySet
    
    keys.toList map { case key =>
      key -> f(d1.getOrElse(key, 0), d2.getOrElse(key, 0))
    } filterNot { _._2 == 0 } toMap
  }

  /**
   * Multiplies this Scalar with another.
   * @param that The Scalar to multiply by
   */
  def *(that: Scalar): Scalar = that match {
    case ExactScalar(r, ds) =>
      ExactScalar(rational * r, combine(_ + _)(decimals, ds))
    case OneUnits => this
    case r : RationalScalar => 
      ExactScalar(rational * r, decimals)
    case _ =>
      val ds = Map() + (that.decimalValue -> 1)
      ExactScalar(rational, combine(_ + _)(decimals, ds))
  }

  /**
   * Adds this Scalar to another.
   * @param that The Scalar to add
   */
  def +(that: Scalar): Scalar = canonicalScalar + that

  /** The additive inverse of this Scalar. */
  override def unary_- : Scalar = ExactScalar(-rational, decimals)

  override def isZero: Boolean = rational.isZero

  /** The decimal value of this Scalar. */
  def decimalValue: BigDecimal = canonicalScalar.decimalValue

}

object ExactScalar {

  def apply(rational: RationalScalar, decimals: (BigDecimal, Int)*): ExactScalar =
    ExactScalar(rational, Map() ++ decimals)

}
