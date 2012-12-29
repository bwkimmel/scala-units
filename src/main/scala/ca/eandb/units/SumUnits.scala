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
 * Represents sums of units.  This is primarily so that sum and difference
 * expressions in the GNU Units language can be parsed without having to resolve
 * referenced units.  We evaluate the sum as soon as some non-trivial operation
 * (like, e.g., printing) is performed on these Units.
 */
case class SumUnits(terms: List[Units]) extends Units {
  private lazy val result = terms reduce (_ + _)
  def canonical = result canonical
  def reciprocal = result reciprocal
  override def root = result root
  override def split = result split
  override def mapScalars(f: Scalar => Scalar) = SumUnits(terms map (_ mapScalars f))
  override def label = terms map (_.label) mkString " + "
  override def termLabel = "(%s)" format label
  override def *(that: Units): Units = SumUnits(terms map (_ * that))
  override def /(that: Units): Units = SumUnits(terms map (_ / that))
  protected[units] def pow(n: Int) = result pow n
}

