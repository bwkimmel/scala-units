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

/** Test suite for operations on Units. */
class UnitsSuite extends FunSuite with BeforeAndAfter {

  var u: UnitsParser = _
  var s: Units = _
  var m: Units = _
  var in: Units = _
  var ft: Units = _

  before {
    u = new UnitsParser
    u.define("s !")
    u.define("m !")
    u.define("c- 1e-2")
    u.define("inch 2.54 cm")
    u.define("foot 12 inch")
    u.define("in inch")
    u.define("ft foot")
    
    s = u("s")
    m = u("m")
    in = u("in")
    ft = u("ft")
  }

  // Even though 12 inches is exactly one foot, if we naively convert to
  // canonical units (meters), divide, then express the result in feet, we get
  // slightly less than 1 ft due to divisions by decimal quantities.  In this
  // case, "12 (in) inOneOf (ft, in)" will result in "12 (in)".  This test
  // ensures that we correctly handle this case and return a result of 1 (ft).
  test("no loss of precision when rational conversion possible") {
    val actual = 12 (in) inOneOf (ft, in)
    val expected = 1 (ft)
    assert(actual === expected)
  }

  test("operator /! allows division of compatible units") {
    assert(2 (ft) /! 6 (in) === IntegerScalar(4))
  }

  test("operator /! does not allow divison of incompatible units") {
    intercept[IncompatibleUnitsException](3 (m) /! 2 (s))
  }

}

