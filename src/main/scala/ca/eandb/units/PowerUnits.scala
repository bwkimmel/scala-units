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
 * Units raised to an exponent.
 * @param base The base Units
 * @param exp The exponent
 */
case class PowerUnits(base: Units, exp: Int) extends Units {
  def label = base match {
    case b : PowerUnits => "(%s)^%d".format(base.label, exp)
    case _ => "%s^%d".format(base.termLabel, exp)
  }

  def canonical = (base.canonical, exp) match {
    case (_, 0) => OneUnits.canonical
    case (b, 1) => b
    case (CanonicalUnits(scale, dims), e) =>
      CanonicalUnits(scale ~ e, dims.mapValues(_ * e))
  }

  def reciprocal = ReciprocalUnits(this)
  protected[units] def pow(n: Int): Units = base match {
    case PowerUnits(b, e) => PowerUnits(b, e * exp * n)
    case _ => PowerUnits(base, exp * n)
  }

  override def mapScalars(f: Scalar => Scalar) = PowerUnits(base mapScalars f, exp)
}

