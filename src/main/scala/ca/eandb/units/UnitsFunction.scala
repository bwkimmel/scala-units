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
 * Represents a function on Units.
 * @param name The name of the function (for parsing and serialization)
 * @param f The underlying mathematical function (on <code>Double</code>s
 */
sealed abstract class UnitsFunction(val name: String, f: Double => Double) {

  /**
   * Extracts the value to supply as input to the underlying function.
   * @param arg The Units argument to the function
   * @return The value to supply as input to the underlying function
   * @throws UnitsResolutionException if <code>arg</code> is incompatible
   *   with this function
   */
  def in(arg: Units): Double

  /**
   * Applies Units to the result of this function.
   * @param result The result from the evaluating the underlying function 
   * @return The return value of this function with Units applied
   */
  def out(result: Double): Units

  /** Evaluates the function. */
  def apply(x: Units): Units = out(f(in(x)))

  /**
   * Binds an argument to this function.
   * @param arg The argument to bind
   * @returns Units that evaluate to this function applied to <code>arg</code>
   */
  def bind(arg: Units): Units = new Units {
    def label = "%s(%s)".format(name, arg.label)
    lazy val canonical = apply(arg).canonical

    override protected[units] def pow(n: Int) = PowerUnits(this, n)
    override def reciprocal = ReciprocalUnits(this)
    override def split = canonical.split
    override def root = canonical.root
    override def isZero = canonical.isZero
    override def mapScalars(map: Scalar => Scalar) = bind(arg mapScalars map)
    override def isScalar = canonical.isScalar
  }

}

/** A UnitsFunction that expects a scalar argument. */
sealed trait ScalarIn {
  def name: String
  def in(arg: Units): Double = arg.asScalarOption match {
    case Some(x) => x.decimalValue.toDouble
    case None =>
      throw new IllegalArgumentException(
        "Argument to function '%s' is not scalar: %s".format(name, arg.toString))
  }
}

/** A UnitsFunction that returns a scalar result. */
sealed trait ScalarOut {
  def out(result: Double): Units = DecimalScalar(BigDecimal(result))
}

/** A UnitsFunction that expects an angular or scalar (radians) argument. */
sealed trait AngleIn {
  def name: String
  def in(arg: Units): Double = {
    val canon = arg.canonical
    val dims = canon.dimensions
    lazy val angle = Map() + (PrimitiveUnits("radian", dimensionless = true) -> 1)

    if (!(dims.isEmpty || dims == angle)) {
      throw new UnitsResolutionException(
        "Argument to function '%s' must be angular or scalar: %s".format(name, arg.toString))
    }

    canon.scale.decimalValue.toDouble
  }
}

/** A UnitsFunction that returns an angular result (in radians). */
sealed trait AngleOut {
  import Helpers._
  def out(result: Double): Units =
    CanonicalUnits(result, PrimitiveUnits("radian", dimensionless = true) -> 1)
}

/** A UnitsFunction that maps scalars to scalars. */
class ScalarFunction(name: String, f: Double => Double)
  extends UnitsFunction(name, f) with ScalarIn with ScalarOut

/** A UnitsFunction that maps angles to scalars. */
class AngleInFunction(name: String, f: Double => Double)
  extends UnitsFunction(name, f) with AngleIn with ScalarOut

/** A UnitsFunction that maps scalars to angles. */
class AngleOutFunction(name: String, f: Double => Double)
  extends UnitsFunction(name, f) with ScalarIn with AngleOut

object UnitsFunction {

  import scala.math._

  /** Built in function defintions. */
  val builtIns: List[UnitsFunction] = List(
    new ScalarFunction("ln", log),
    new ScalarFunction("log", log10),
    new ScalarFunction("log2", log(_) / log(2)),
    new ScalarFunction("exp", exp),
    new AngleInFunction("sin", sin),
    new AngleInFunction("cos", cos),
    new AngleInFunction("tan", tan),
    new AngleOutFunction("asin", asin),
    new AngleOutFunction("acos", acos),
    new AngleOutFunction("atan", atan))

}

