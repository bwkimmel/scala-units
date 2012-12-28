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

import Helpers._

/**
 * Units raised to an exponent.
 * @param base The base Units
 * @param exp The exponent
 */
case class DeferredPowerUnits(base: Units, exp: Units) extends Units {
  def label = base match {
    case b : PowerUnits => "(%s)^%s".format(base.label, exp.termLabel)
    case _ => "%s^%s".format(base.termLabel, exp.termLabel)
  }

  override def termLabel = "(%s^%s)".format(base.termLabel, exp.termLabel)

  private def resolveExponent = exp.asScalarOption match {
    case Some(x) => x.canonicalScalar
    case None =>
      throw new UnitsResolutionException("Non-scalar exponent: %s".format(label))
  }

  def canonical = (base, resolveExponent) match {
    case (_, x) if x isZero => OneUnits.canonical
    case (b, OneUnits) => b.canonical
    case (b, IntegerScalar(n)) => b pow n.toInt canonical
    case (b, e @ RationalScalar(n, d)) =>
      val canon = b.canonical
      val dims = canon.dimensions
      
      if (dims exists { case (_, e) => (e * n) % d != 0 }) {
        throw new UnitsResolutionException(
          "Attempt to raise non-scalar units to an exponent that yields non-integral powers")
      }

      val b0 = canon.scale.decimalValue.toDouble
      val e0 = e.decimalValue.toDouble
      val scale = math.pow(b0, e0)

      CanonicalUnits(scale, dims map { case (b, e) => b -> (e * n / d).toInt })

    case (b, e) =>
      val e0 = e.decimalValue.toDouble
      val b0 = b.asScalarOption match {
        case Some(x) => x.decimalValue.toDouble
        case None =>
          throw new UnitsResolutionException(
            "Attempt to raise non-scalar units to a decimal power")
      }

      math.pow(b0, e0) canonical
  }

  def reciprocal = ReciprocalUnits(this)
  def pow(n: Int): Units = base match {
    case PowerUnits(b, e) => DeferredPowerUnits(b, e * exp * n)
    case DeferredPowerUnits(b, e) => DeferredPowerUnits(b, e * exp * n)
    case _ => DeferredPowerUnits(base, exp * n)
  }

  override def mapScalars(f: Scalar => Scalar) =
    DeferredPowerUnits(base mapScalars f, exp mapScalars f)
}

object DeferredPowerUnits {

  class Sqrt(arg: Units) extends DeferredPowerUnits(arg, RationalScalar(1, 2)) {
    override def termLabel = "sqrt(%s)".format(arg.label)
    override def label = termLabel
  }

  class CubeRoot(arg: Units) extends DeferredPowerUnits(arg, RationalScalar(1, 3)) {
    override def termLabel = "cuberoot(%s)".format(arg.label)
    override def label = termLabel
  }

}

