Scala Units
===========

Project for run-time interpretation, conversion, and analysis of units
in Scala.

Example:

```scala
val u = new UnitsParser
// ... load units ...

val v = 80 * u("km/hr") 
val t = 5 * u("min")
val d = v * t

println(d in u("miles"))  // => 4.1424746 miles
```


Importing Units
---------------

Units may be loaded from [GNU Units](http://www.gnu.org/software/units/)
definition files.

```scala
val u = new UnitsParser
val f = Source.fromFile("/usr/share/units/definitions.units", "utf-8")

u.load(f)
```

Limitations:

  - !include directives are not followed.
  - Non-linear units (i.e., function definitions) are not supported.

Definitions may be added manually:

```scala
u.define("fortnight 2 weeks")
u.define("quatloo !")
```

The definition should follow the GNU Units
[definition](http://www.gnu.org/software/units/manual/units.html#Unit-Definitions)
format, subject to the limitations above.


Parsing Units
-------------

Units may be parsed from the GNU Units
[expression](http://www.gnu.org/software/units/manual/units.html#Unit-Expressions)
format, subject to the limitations above.

```scala
val x = u("1|3 m / s2")
```


Common Units
------------

Common units may be imported for convenience:

```scala
val u = new UnitsParser
// ... load units ...

val common = new CommonUnits(u)
import common._

val g = 9.8 (m / s~2)
val e = 1.3 (kW) * 8 (hours) in J
```


Unit Conversion
---------------

### Basic unit conversion

To convert a value to different units, use the `in` operator:

```scala
val x = 1 (m) in ft 
println(x)  // => 3.280839 ft
```

### Natural units

To convert a value to the most natural of units from a sequence, use
the `inOneOf` operator with multiple units in descending order by
magnitude.  The units selected will be the first units for which the
numerical value is at least 1, or the last units if none of the provided
units yield a numerical value at least 1.

```scala
val x = 90 (seconds) inOneOf (hour, min, s)
println(x)  // => 1.5 min

val x = 2100 (MHz) inOneOf (GHz, MHz, kHz, Hz)
println(x)  // => 2.1 GHz

val x = 1 (mL) inOneOf (gal, quart, pint, cups, tbsp, tsp)
println(x)  // => 0.2 tsp
```

### Compound units

To convert a value to compound units (e.g., hours, minutes, seconds),
use the `in` operator with multiple units in descending order by
magnitude.  Each component will have an integral numerical value, except
potentially the last component.  Zero-valued components will be dropped.

```scala
val x = 90.2 (seconds) in (hour, min, s)
println(x)  // => List(1 min, 30.2 s)

val x = 3602 (seconds) in (hour, min, s)
println(x)  // => List(1 hour, 2 s)
```

To break up the last component into integral and fractional parts,
simply duplicate the last units:

```scala
val x = 90.2 (seconds) in (hour, min, s, s)
println(x)  // => List(1 min, 30 s, 0.2 s)
```

To include zero-valued components, use `inAllOf`:

```scala
val x = 90.2 (seconds) inAllOf (hour, min, s)
println(x)  // => List(0 hour, 1 min, 30.2 s)

val x = 3602 (seconds) inAllOf (hour, min, s)
println(x)  // => List(1 hour, 0 min, 2 s)
```


Dimensional Analysis
--------------------

### Canonical Form

Units may be converted to canonical form using the `canonical` property.
Canonical units are expressed as a scalar value and powers of primitive
units.

```scala
println(kW canonical)  // => 1000 kg m^2 s^-3
```

Values may be converted between units if, in their canonical form, the
exponents on all primitive units are identical.  The `dimensions`
property may be used to obtain a the exponents in the form of a Map:

```scala
println(kW dimensions)  // => Map(kg -> 1, m -> 2, s -> -3)
```

### Testing for Compatibility

To test for compatibility between units, use the `is` operator.

```scala
val x = 55 (miles / hour)
println(x is meter / second)  // => true
println(x is kW)              // => false
```

CommonUnits provides pseudo-units for classes of units (time, length,
area, etc), to make dimensional analysis more convenient/readable.

```scala
val x = 55 (miles / hour)
println(x is speed)  // => true
println(x is power)  // => false

println(speed is time / length)  // => true
println(mpg is 1 / area)         // => true (yes.. it really is)
```

Contrary to the popular refrain:

    scala> time is money
    res8: Boolean = false


Operations on Units
-------------------

### Multiplication and Division

Any two units may be multiplied or divided.  Multiplication uses the `*`
operator.

```scala
println(2 (m) * 3 (ft) in sqft)  // => 19.685 ft^2
println(60 (W) * 8 (h) in btu)   // => 1637.828 btu
println(1 (kW * h) in J)         // => 3600000 J
```

Division uses `/` or `per`:

```scala
println(2000 (kcal) / 24 (hours) in W)  // => 96.917 W
println(55 (miles per hour) in km / h)  // => 88.514 km / hour
```

Without specifying a unit conversion, the result will directly reflect
the requested product or quotient.

```scala
println(2 (m) * 3(ft))             // => 2 m 3 ft
println(2000 (kcal) / 24 (hours))  // => 2000 kcal / (24 hour)
```


### Exponentiation

Any unit may be raised to an integer power using the `~` operator.

```scala
println(2 (ft~3) in L)                     // => 56.634 L
println(60 (mph) / 4.2 (s) in m * s~(-2))  // => 6.386 m / s^2
```

The `~` operator is used instead of `^` because `~` has the correct
precedence for exponentiation.


### Addition and Subtraction

Compatible units may be added or subtracted using the `+` and `-`
operators.  If no conversion is specified, the result will be expressed
in the units of the left operand.

```scala
println(1 (min) + 30 (s))  // => 1.5 min
println(30 (s) + 1 (min))  // => 90 s
println(80 (kg) + 3 (s))   // => throws IncompatibleUnitsException
```


### Comparisons

Compatible units may be compared

```scala
println(km > mile)      // => false
println(ft <= 12 (in))  // => true
println(km > hour)      // => throws IncompatibleUnitsException
```


License
-------

Copyright (c) 2012 Bradley W. Kimmel

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

