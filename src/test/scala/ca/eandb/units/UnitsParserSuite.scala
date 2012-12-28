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

  test("[2.0] named dimensionless units cannot be used in exponents") {
    u.define("meter !")
    u.define("radian !dimensionless")
    intercept[UnitsResolutionException] { u("meter^radian").canonical }
  }

  test("[4.0] tries removing trailing 's'") {
    u.define("meter !")
    assert(u("meters").canonical === CanonicalUnits(OneUnits, PrimitiveUnits("meter") -> 1))
  }

  test("[4.0] don't remove trailing 's' if plural unit defined separately") {
    u.define("meter !")
    u.define("meters !")
    assert(u("meters").canonical === CanonicalUnits(OneUnits, PrimitiveUnits("meters") -> 1))
  }

  test("[4.0] tries removing trailing 'es'") {
    u.define("inch !")
    assert(u("inches").canonical === CanonicalUnits(OneUnits, PrimitiveUnits("inch") -> 1))
  }

  test("[4.0] don't remove trailing 'es' if plural unit defined separately") {
    u.define("inch !")
    u.define("inches !")
    assert(u("inches").canonical === CanonicalUnits(OneUnits, PrimitiveUnits("inches") -> 1))
  }

  test("[4.0] tries removing trailing 's' before removing trailing 'es'") {
    u.define("inch !")
    u.define("inche !")
    assert(u("inches").canonical === CanonicalUnits(OneUnits, PrimitiveUnits("inche") -> 1))
  }

  test("[4.0] tries replacing trailing 'ies' with 'y'") {
    u.define("century !")
    assert(u("centuries").canonical === CanonicalUnits(OneUnits, PrimitiveUnits("century") -> 1))
  }

  test("[4.0] don't replace trailing 'ies' with 'y' if plural unit defined separately") {
    u.define("century !")
    u.define("centuries !")
    assert(u("centuries").canonical === CanonicalUnits(OneUnits, PrimitiveUnits("centuries") -> 1))
  }

  test("[4.0] tries removing trailing 's' before replacing trailing 'ies' with 'y'") {
    u.define("century !")
    u.define("centurie !")
    assert(u("centuries").canonical === CanonicalUnits(OneUnits, PrimitiveUnits("centurie") -> 1))
  }

  test("[4.0] tries removing trailing 'es' before replacing trailing 'ies' with 'y'") {
    u.define("century !")
    u.define("centuri !")
    assert(u("centuries").canonical === CanonicalUnits(OneUnits, PrimitiveUnits("centuri") -> 1))
  }

  test("[4.0] only one prefix is permitted per unit") {
    u.define("farad !")
    u.define("micro- 0.000001")
    assert(u("microfarad").canonical ===
      CanonicalUnits(DecimalScalar(BigDecimal("0.000001")), PrimitiveUnits("farad") -> 1))
    intercept[UndefinedUnitsException] { u("micromicrofarad").canonical }
  }

  test("[4.0] prefer exact definition over use of a prefix") {
    u.define("mounce !")  // (metric ounce) == 25 g
    u.define("ounce !")   // 1|16 pound
    u.define("m- 0.001")
    assert(u("mounce").canonical === CanonicalUnits(OneUnits, PrimitiveUnits("mounce") -> 1))
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

  test("[5.1] can multiply units using a space") {
    u.define("m !")
    u.define("s !")
    assert(u("m s").canonical ===
      CanonicalUnits(OneUnits,
        PrimitiveUnits("m") -> 1,
        PrimitiveUnits("s") -> 1))
  }

  test("[5.1] can multiply units using an asterisk ('*')") {
    u.define("m !")
    u.define("s !")
    assert(u("m * s").canonical ===
      CanonicalUnits(OneUnits,
        PrimitiveUnits("m") -> 1,
        PrimitiveUnits("s") -> 1))
  }

  test("[5.1] can divide units using a slash ('/')") {
    u.define("m !")
    u.define("s !")
    assert(u("m/s").canonical ===
      CanonicalUnits(OneUnits,
        PrimitiveUnits("m") -> 1,
        PrimitiveUnits("s") -> -1))
  }

  test("[5.1] can divide units using 'per'") {
    u.define("mile !")
    u.define("hour !")
    assert(u("mile per hour").canonical ===
      CanonicalUnits(OneUnits,
        PrimitiveUnits("mile") -> 1,
        PrimitiveUnits("hour") -> -1))
  }

  test("[5.1] multiplication using space has a higher precedence than division using a slash") {
    u.define("W !")
    u.define("m !")
    u.define("Hz !")
    u.define("s !")
    u.define("day 86400 s")

    // First example
    assert(u("W / m^2 Hz").canonical ===
      CanonicalUnits(OneUnits,
        PrimitiveUnits("W") -> 1,
        PrimitiveUnits("m") -> -2,
        PrimitiveUnits("Hz") -> -1))

    // Second example
    assert(u("m/s s/day").canonical === u("m / s s day").canonical)

    // Third example
    assert(u("1/2 m").canonical ===
      CanonicalUnits(RationalScalar(1, 2), PrimitiveUnits("m") -> -1))
  }

  test("[5.1] multiplication using '*' has same precedence as division using '/'") {
    u.define("m !")
    u.define("s !")
    u.define("day 86400 s")

    assert(u("1/2 * m").canonical ===
      CanonicalUnits(RationalScalar(1, 2), PrimitiveUnits("m") -> 1))

    assert(u("m/s * s/day").canonical ===
      CanonicalUnits(RationalScalar(1, 86400),
        PrimitiveUnits("m") -> 1,
        PrimitiveUnits("s") -> -1))
  }

  test("[5.1] division with | has higher precedence than multiplication using space") {
    u.define("m !")
    assert(u("1|2 m").canonical ===
      CanonicalUnits(RationalScalar(1, 2), PrimitiveUnits("m") -> 1))
  }

  test("[5.1] division with | has higher precedence than exponentiation") {
    assert(u("2|3^1|2").canonical ===
      CanonicalUnits(DecimalScalar(BigDecimal(math.sqrt(2.0 / 3.0)))))
  }

  test("[5.1] cannot use vertical bar to indicate division of non-numerical units") {
    u.define("m !")
    u.define("s !")
    intercept[UnitsParsingException] { u("m|s") }
  }

  test("[5.1] can use parentheses for grouping") {
    u.define("W !")
    u.define("kg !")
    u.define("m !")
    u.define("Hz !")
    assert(u("(1/2) kg / (kg/m)").canonical ===
      CanonicalUnits(RationalScalar(1, 2), PrimitiveUnits("m") -> 1))
    assert(u("W / (m^2 * Hz)").canonical ===
      CanonicalUnits(OneUnits,
        PrimitiveUnits("W") -> 1,
        PrimitiveUnits("m") -> -2,
        PrimitiveUnits("Hz") -> -1))
  }

  test("[5.2] can parse sums of units using '+' sign") {
    u.define("in !")
    u.define("ft 12 in")
    assert(u("12 ft + 3 in + 3|8 in").canonical ===
      CanonicalUnits(RationalScalar(1179, 8), PrimitiveUnits("in") -> 1))
  }

  test("[5.2] sums must have compatible units") {
    u.define("m !")
    u.define("s !")
    intercept[IncompatibleUnitsException] { u("2 m + 3 s").canonical }
    intercept[IncompatibleUnitsException] { u("2 m + 3 m^2").canonical }
    intercept[IncompatibleUnitsException] { u("2 m + 3").canonical }
  }

  test("[5.3] numbers can appear many times and in any order") {
    u.define("ft !")
    u.define("yard 3 ft")
    u.define("$ !")
    assert(u("2 ft 3 ft 12 ft").canonical ===
      CanonicalUnits(IntegerScalar(72), PrimitiveUnits("ft") -> 3))
    assert(u("$ 5 / yard").canonical ===
      CanonicalUnits(RationalScalar(5, 3),
        PrimitiveUnits("$") -> 1,
        PrimitiveUnits("ft") -> -1))
  }

  val builtIns = List("ln", "log", "log2", "exp", "sin", "cos", "tan", "asin", "acos", "atan")
  builtIns foreach { fun =>
    test("[5.4] can parse %s".format(fun)) {
      u("%s(0)".format(fun))
    }
  }

  List("sin", "cos", "tan") foreach { fun =>
    test("[5.4] %s accepts dimensionless argument or an argument with dimensions of angle"
        .format(fun)) {

      u.define("radian !dimensionless")

      u("%s(1)".format(fun)).canonical
      u("%s(1 radian)".format(fun)).canonical
    }

    test("[5.4] %s rejects dimensionless arguments other than angles".format(fun)) {
      u.define("radian !dimensionless")
      u.define("sr !dimensionless")

      intercept[UnitsResolutionException] { u("%s(1 radian^2)".format(fun)).canonical }
      intercept[UnitsResolutionException] { u("%s(1 sr)".format(fun)).canonical }
    }

    test("[5.4] %s rejects non-dimensionless arguments".format(fun)) {
      u.define("m !")

      intercept[UnitsResolutionException] { u("%s(1 m)".format(fun)).canonical }
    }
  }

  List("asin", "acos", "atan") foreach { fun =>
    test("[5.4] %s returns arguments with dimensions of angle".format(fun)) {
      u.define("radian !dimensionless")

      val dims = Map() + (PrimitiveUnits("radian") -> 1)
      assert(u("%s(0)".format(fun)).dimensions === dims)
    }
  }

  test("[5.4] can parse sqrt") { u("sqrt(0)") }
  test("[5.4] can parse cuberoot") { u("cuberoot(0)") }

  test("[5.4] can take sqrt of dimensionless quantity") {
    assert(u("sqrt(4)").canonical === CanonicalUnits(IntegerScalar(2)))
  }

  test("[5.4] can take cuberoot of dimensionless quantity") {
    assert(u("cuberoot(8)").canonical === CanonicalUnits(IntegerScalar(2)))
  }

  test("[5.4] can take sqrt of squared units") {
    u.define("m !")
    u.define("s !")
    assert(u("sqrt(4 m^2)").canonical ===
      CanonicalUnits(IntegerScalar(2), PrimitiveUnits("m") -> 1))
    assert(u("sqrt(4 m^2 s^4)").canonical ===
      CanonicalUnits(IntegerScalar(2), PrimitiveUnits("m") -> 1, PrimitiveUnits("s") -> 2))
  }

  test("[5.4] can take sqrt of product units having squared dimensions") {
    u.define("in !")
    u.define("ft 12 in")
    assert(u("sqrt(3 ft in)").canonical ===
      CanonicalUnits(IntegerScalar(6), PrimitiveUnits("in") -> 1))
  }

  test("[5.4] can take cuberoot of cubed units") {
    u.define("m !")
    u.define("s !")
    assert(u("cuberoot(8 m^3)").canonical ===
      CanonicalUnits(IntegerScalar(2), PrimitiveUnits("m") -> 1))
    assert(u("cuberoot(8 m^3 s^6)").canonical ===
      CanonicalUnits(IntegerScalar(2), PrimitiveUnits("m") -> 1, PrimitiveUnits("s") -> 2))
  }

  test("[5.4] can take cuberoot of product units having cubed dimensions") {
    u.define("in !")
    u.define("ft 12 in")
    assert(u("cuberoot(18 ft in^2)").canonical ===
      CanonicalUnits(IntegerScalar(6), PrimitiveUnits("in") -> 1))
  }

  test("[5.4] cannot take sqrt of non-square units") {
    u.define("m !")
    u.define("s !")
    intercept[UnitsResolutionException] { u("sqrt(1 m)").canonical }
    intercept[UnitsResolutionException] { u("sqrt(1 m s)").canonical }
    intercept[UnitsResolutionException] { u("sqrt(1 m^3)").canonical }
  }

  test("[5.4] cannot take cuberoot of non-cube units") {
    u.define("m !")
    u.define("s !")
    u.define("kg !")
    intercept[UnitsResolutionException] { u("sqrt(1 m)").canonical }
    intercept[UnitsResolutionException] { u("sqrt(1 m s)").canonical }
    intercept[UnitsResolutionException] { u("sqrt(1 m s^2)").canonical }
    intercept[UnitsResolutionException] { u("sqrt(1 m^3 s)").canonical }
    intercept[UnitsResolutionException] { u("sqrt(1 kg m s)").canonical }
  }

  test("[5.4] can obtain higher roots of dimensionless quantities") {
    assert(u("16^(1|4)").canonical === CanonicalUnits(IntegerScalar(2)))
    assert(u("81^(1|4)").canonical === CanonicalUnits(IntegerScalar(3)))
  }

  test("[5.4] can obtain nth roots on quantity having dimensions raised to n") {
    u.define("m !")
    u.define("s !")
    assert(u("(1 m^24 s^18)^(1|6)").canonical ===
      CanonicalUnits(OneUnits, PrimitiveUnits("m") -> 4, PrimitiveUnits("s") -> 3))
    assert(u("(1 m^4 s^2)^(3|2)").canonical ===
      CanonicalUnits(OneUnits, PrimitiveUnits("m") -> 6, PrimitiveUnits("s") -> 3))
  }

  test("[5.4] cannot obtain nth roots on quantity not having dimensions raised to n") {
    u.define("m !")
    u.define("A !")
    u.define("cd !")
    u.define("s !")
    u.define("kg !")
    u.define("K !")
    intercept[UnitsResolutionException] { u("(1 m)^(1|6)").canonical }
    intercept[UnitsResolutionException] { u("(1 m s kg K A cd)^(1|6)").canonical }
    intercept[UnitsResolutionException] { u("(1 m^3 s^3)^(1|6)").canonical }
    intercept[UnitsResolutionException] { u("(1 m^5)^(1|6)").canonical }
    intercept[UnitsResolutionException] { u("(1 m)^(3|2)").canonical }
    intercept[UnitsResolutionException] { u("(1 m s)^(3|2)").canonical }
    intercept[UnitsResolutionException] { u("(1 m^3)^(3|2)").canonical }
  }

  List("+", "-", "*", "/", "|", "^", ";", "~", "#", "(", ")") foreach { op =>
    test("[9.2] unit names must not contain '%s'".format(op)) {
      intercept[CommandParsingException] { u.define("foo%sbar !".format(op)) }
    }
  }

  List("_", ",", ".") foreach { sym =>
    test("[9.2] unit names must not begin with '%s'".format(sym)) {
      intercept[CommandParsingException] { u.define("%sbar !".format(sym)) }
    }
    test("[9.2] unit names may contain '%s' in the middle".format(sym)) {
      u.define("foo%sbar !".format(sym))
      assert(u("foo%sbar".format(sym)).canonical ===
        CanonicalUnits(OneUnits, PrimitiveUnits("foo%sbar".format(sym)) -> 1))
    }
    test("[9.2] unit names must not end with '%s'".format(sym)) {
      intercept[CommandParsingException] { u.define("foo%s !".format(sym)) }
    }
  }

  test("[9.2] names cannot begin with a digit") {
    for (i <- 0 to 9)
      intercept[CommandParsingException] { u.define("%dbar !".format(i)) }
  }

  test("[9.2] name ending in single digit without preceding underscore is invalid") {
    intercept[CommandParsingException] { u.define("foo2 !") }
  }

  test("[9.2] name ending in single digit not preceded by '_' followed by only digits, commas, decimal points is invalid") {
    intercept[CommandParsingException] { u.define("foo_a2 !") }
  }

  test("[9.2] name ending in digit preceded by '_' followed by only digits, commas, decimal points valid") {
    List("foo_2", "foo_2,1", "foo_3.14", "N2O", "NO_2") foreach { name =>
      u.define("%s !".format(name))
      assert(u(name).canonical ===
        CanonicalUnits(OneUnits, PrimitiveUnits(name) -> 1))
    }
  }

}

