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

/** Represents a scalar function evaluation */
class ScalarFunction(name: String, f: Double => Double, arg: Units) extends Scalar {

  override def termLabel = "%s(%s)".format(name, arg.label)
  def label = termLabel

  /** Expresses this Scalar in canonical form. */
  def canonicalScalar: Scalar = DecimalScalar(decimalValue)

  override def reciprocal: Scalar = canonicalScalar.reciprocal
  override def pow(n: Int): Scalar = canonicalScalar pow n

  /** Splits this Scalar into integral and fractional parts. */
  def truncate: (IntegerScalar, Scalar) = canonicalScalar.truncate

  /**
   * Multiplies this Scalar with another.
   * @param that The Scalar to multiply by
   */
  def *(that: Scalar): Scalar = canonicalScalar * that

  /**
   * Adds this Scalar to another.
   * @param that The Scalar to add
   */
  def +(that: Scalar): Scalar = canonicalScalar + that

  /** The additive inverse of this Scalar. */
  override def unary_- : Scalar = -canonicalScalar

  override def isZero: Boolean = canonicalScalar isZero

  /** The decimal value of this Scalar. */
  def decimalValue: BigDecimal = arg.asScalarOption match {
    case Some(scalar) =>
      val x = scalar.decimalValue.toDouble
      BigDecimal(f(x))
    case None =>
      throw new IllegalArgumentException(
        "Argument to function '%s' is not scalar: %s".format(name, arg.toString))
  }

}

object ScalarFunction {

  import scala.math._

  class Definition(val name: String, f: Double => Double) extends Function1[Units, ScalarFunction] {
    def apply(arg: Units) = new ScalarFunction(name, f, arg)
  }

  object Log extends Definition("ln", log)
  object Log10 extends Definition("log", log10)
  object Log2 extends Definition("log2", log(_) / log(2))
  object Exp extends Definition("exp", exp)
  object Sin extends Definition("sin", sin)
  object Cos extends Definition("cos", cos)
  object Tan extends Definition("tan", tan)
  object ASin extends Definition("asin", asin)
  object ACos extends Definition("acos", acos)
  object ATan extends Definition("atan", atan)

  val builtIns: List[Definition] = List(
    Log, Log10, Log2, Exp, Sin, Cos, Tan, ASin, ACos, ATan)

}
