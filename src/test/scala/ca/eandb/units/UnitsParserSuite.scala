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

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

import Helpers._

class UnitsParserSuite extends FunSuite with BeforeAndAfter {

  var u: UnitsParser = _

  before {
    u = new UnitsParser
  }

  test("[9.2] can define new primitive units") {
    u.define("quatloo !")
    assert(u("quatloo").canonical === CanonicalUnits(OneUnits, PrimitiveUnits("quatloo") -> 1))
  }

  test("[9.2] can define new dimensionless units") {
    u.define("radians !dimensionless")
    assert(u("radians").canonical === CanonicalUnits(OneUnits))
  }

  test("[9.2] a unit that ends with a '-' character is a prefix") {
    u.define("m !")
    u.define("c- 0.01")
    assert(u("cm").canonical ===
      CanonicalUnits(DecimalScalar(BigDecimal("0.01")), PrimitiveUnits("m") -> 1))
  }

  test("[5.1] when unit includes a prefix, exponent operators apply to the combination") {
    u.define("m !")
    u.define("c- 0.01")
    val actual = u("cm^3").canonical.withCanonicalScalars
    val expected = CanonicalUnits(DecimalScalar(BigDecimal("0.000001")), PrimitiveUnits("m") -> 3)
    assert(actual === expected)
  }

  test("[5.1] if you separate the prefix from the unit with any multiplication operator, then the prefix is treated as a separate unit") {
    u.define("m !")
    u.define("c- 0.01")
    val expected = CanonicalUnits(DecimalScalar(BigDecimal("0.01")), PrimitiveUnits("m") -> 3)
    assert(u("c m^3").canonical === expected)
    assert(u("c * m^3").canonical === expected)
  }

  test("[5.1] cannot use vertical bar to indicate division of non-numerical units") {
    u.define("m !")
    u.define("s !")
    intercept[UnitsParsingException] { u("m|s") }
  }

  test("[5.1] interpret concatenation of unit with digit as exponent") {
    u.define("m !")
    assert(u("m3").canonical === CanonicalUnits(OneUnits, PrimitiveUnits("m") -> 3))
  }

  test("[5.1] exponentiation is right-associative") {
    assert(u("2^3^2").canonical === CanonicalUnits(IntegerScalar(512)))
  }

  test("[5.1] can use ** as an exponent operator") {
    assert(u("3**3").canonical === CanonicalUnits(IntegerScalar(27)))
  }

  test("division with | has higher precedence than multiplication by adjacency") {
    u.define("m !")
    assert(u("1|2 m").canonical.withCanonicalScalars === u("(1|2) m").canonical.withCanonicalScalars)
  }

  val builtIns = List("ln", "log", "log2", "exp", "sin", "cos", "tan", "asin", "acos", "atan")
  builtIns foreach { fun =>
    test("[5.4] can parse %s".format(fun)) {
      u("%s(1)".format(fun))
    }
  }

}

