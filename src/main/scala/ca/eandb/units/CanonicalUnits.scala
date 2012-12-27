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
 * Represents units in a form that can be readily compared for compatibility,
 * and which allows direct conversion between instances.
 */
case class CanonicalUnits(scale: Scalar, override val dimensions: Map[PrimitiveUnits, Int] = Map.empty)
    extends Units {

  // Require that no dimensions have zero exponents, so that we can test
  // compatibility by comparing these Maps directly.
  require(dimensions.values.forall(_ != 0), "All exponents must be non-zero")

  def canonical = this
  override def isScalar = dimensions.isEmpty
  override def root = CanonicalUnits(OneUnits, dimensions)
  override def split = (scale, root)

  override def mapScalars(f: Scalar => Scalar) = CanonicalUnits(f(scale), dimensions)

  /** Expresses these Units as products of powers of primitive Units. */
  def expand: Units = {
    val dims = dimensions.toList map {
      case (b, 1) => b
      case (b, e) => PowerUnits(b, e)
    }
    val terms = scale :: dims

    terms.reduce(_ * _)
  }

  def *(that: CanonicalUnits): CanonicalUnits = {
    val s = scale * that.scale
    val dims =
      dimensions.keySet union that.dimensions.keySet flatMap { key =>
        val exp = dimensions.getOrElse(key, 0) + that.dimensions.getOrElse(key, 0)
        if (exp != 0) Some(key -> exp) else None
      } toMap

    CanonicalUnits(s, dims)
  }

  override def *(that: Units): Units = that match {
    case u : CanonicalUnits => this * u
    case s : Scalar => CanonicalUnits(scale * s, dimensions)
    case u => super.*(u)
  }

  def reciprocal = CanonicalUnits(scale.reciprocal, dimensions.mapValues(-_))
  def pow(n: Int) = CanonicalUnits(scale pow n, dimensions.mapValues(_ * n))

  def label = expand.label
}

