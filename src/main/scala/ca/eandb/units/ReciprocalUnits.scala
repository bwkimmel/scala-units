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
 * The multiplicative inverse of another Units.
 * @param u The reciprocal of these Units
 */
case class ReciprocalUnits(u: Units) extends Units {
  def label = "/ %s".format(u.termLabel)
  override def termLabel = "(%s)".format(label)
  def canonical =
    PowerUnits(u, -1).canonical
  override def root = if (u isScalar) OneUnits else ReciprocalUnits(u root)
  override def split = u.split match {
    case (scale, d) => (scale.reciprocal, d.reciprocal)
  }
  override def mapScalars(f: Scalar => Scalar) = ReciprocalUnits(u mapScalars f)
  def reciprocal = u
  protected[units] def pow(n: Int) = ReciprocalUnits(u pow n)
}

