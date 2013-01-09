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
 * The product of a set of Units.
 * @param terms The list of Units being multiplied
 */
case class ProductUnits(terms: List[Units]) extends Units {
  def label = {
    val result = new StringBuilder
    def build(ts: List[Units]): Unit = ts match {
      case Nil =>
      case (t : ReciprocalUnits) :: Nil =>
        result.append(t.label)
      case (t : ReciprocalUnits) :: ReciprocalUnits(u) :: rest =>
        result.append(t.label)
        result.append(" / ")
        build(u :: rest)
      case (t : ReciprocalUnits) :: rest =>
        result.append(t.label)
        result.append(" * ")
        build(rest)
      case t :: Nil =>
        result.append(t.termLabel)
      case t :: rest =>
        result.append(t.termLabel)
        result.append(" ")
        build(rest)
    }
    build(terms)
    result.toString
  }

  override def convert(that: Units): ProductUnits = terms match {
    case OneUnits :: rest => ProductUnits((that /! this) :: rest)
    case _ => ProductUnits((that /! this) :: terms)
  }

  override def termLabel = "(%s)".format(label)

  def canonical = terms.map(_.canonical).reduce(_ * _)

  override def root = (terms.map(_.root).filterNot(_.isScalar) map {
    case t: PowerUnits => t
    case t => PowerUnits(t, 1)
  } groupBy { _.base } mapValues { _.map(_.exp).sum } flatMap {
    case (b, 0) => None
    case (b, 1) => Some(b)
    case (b, e) => Some(PowerUnits(b, e))
  }).toList match {
    case Nil => OneUnits
    case t :: Nil => t
    case ts => ProductUnits(ts)
  }

  override def split = {
    val parts = terms.map(_.split)
    val scale = parts.map(_._1).reduce(_ * _)

    (scale, ProductUnits(parts.map(_._2)).root)
  }

  override def mapScalars(f: Scalar => Scalar) =
    ProductUnits(terms map (_ mapScalars f))

  override def *(that: Units): Units = that match {
    case ProductUnits(rterms) => ProductUnits(terms ::: rterms)
    case _ => ProductUnits(terms :+ that)
  }

  protected[units] def pow(n: Int) = PowerUnits(this, n)
  def reciprocal = ReciprocalUnits(this)
}

