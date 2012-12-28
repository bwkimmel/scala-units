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
 * Variable definitions for common units.  This class is intended to be
 * imported to provide convenient reference to common units.  All fields are
 * lazy so that units which have not been defined in the underlying
 * UnitsParser will cause problems if they are not referenced.
 *
 * Example usage:
 *
 * <pre>
 *   val units = new UnitsParser
 *   // ... load units ...
 *   val common = new CommonUnits(units)
 *   
 *   import common._
 *
 *   val x = 100 (km / h) in mph
 * </pre>
 */
class CommonUnits(units: UnitsParser) {

  //----------------------------------------------------------------
  // Names of common unit classes -- useful for dimensional analysis
  //
  // Example usage:
  //   val x = 9.8 (m / s / s) is accleration
  //   => true
  //   
  //   val x = 250 (mL) is area
  //   => false
  //

  lazy val time = units("TIME")
  lazy val length = units("LENGTH")
  lazy val area = units("AREA")
  lazy val volume = units("VOLUME")
  lazy val mass = units("MASS")
  lazy val current = units("CURRENT")
  lazy val amount = units("AMOUNT")
  lazy val angle = units("ANGLE")
  lazy val solidAngle = units("SOLID_ANGLE")
  lazy val money = units("MONEY")
  lazy val energy = units("ENERGY")
  lazy val power = units("POWER")
  lazy val force = units("FORCE")
  lazy val pressure = units("PRESSURE")
  lazy val frequency = units("FREQUENCY")
  lazy val velocity = units("VELOCITY")
  lazy val speed = velocity
  lazy val acceleration = units("ACCELERATION")
  lazy val density = units("DENSITY")
  lazy val temperature = units("TEMPERATURE")
  lazy val luminousIntensity = units("LUMINOUS_INTENSITY")
  lazy val luminousFlux = units("LUMINOUS_FLUX")
  lazy val illuminance = units("ILLUMINANCE")
  lazy val luminance = units("LUMINANCE")
  lazy val information = units("INFORMATION")
  lazy val entropy = units("ENTROPY")

  //----------------------------------------------------------------

  lazy val radian = units("radian")
  lazy val radians = radian

  lazy val deg = units("deg")
  lazy val degree = deg
  lazy val degrees = deg

  lazy val arcmin = units("arcmin")
  lazy val arcsec = units("arcsec")

  lazy val sr = units("sr")
  lazy val steradian = sr

  lazy val m = units("m")
  lazy val meter = m
  lazy val meters = m

  lazy val nm = units("nm")
  lazy val um = units("um")
  lazy val mm = units("mm")
  lazy val cm = units("cm")
  lazy val km = units("km")

  lazy val hour = units("hour")
  lazy val hours = hour
  lazy val h = hour

  lazy val min = units("min")
  lazy val minute = min
  lazy val minutes = min

  lazy val s = units("s")
  lazy val sec = s
  lazy val second = s
  lazy val seconds = s

  lazy val day = units("day")
  lazy val days = units("days")

  lazy val week = units("week")
  lazy val weeks = units("weeks")

  lazy val year = units("year")
  lazy val years = units("years")

  lazy val mile = units("mile")
  lazy val miles = mile

  lazy val ft = units("ft")
  lazy val foot = ft
  lazy val feet = ft

  lazy val inch = units("inch")
  lazy val inches = inch
  lazy val in = inch

  lazy val yd = units("yd")
  lazy val yard = yd
  lazy val yards = yd

  lazy val L = units("L")
  lazy val litre = L
  lazy val litres = L

  lazy val mL = units("mL")
  lazy val dL = units("dL")
  
  lazy val kg = units("kg")
  lazy val g = units("g")
  lazy val mg = units("mg")
  
  lazy val gram = g
  lazy val grams = g

  lazy val lb = units("lb")
  lazy val lbs = lb
  lazy val pound = lb
  lazy val pounds = lb

  lazy val cd = units("cd")
  lazy val lx = units("lx")

  lazy val A = units("A")
  lazy val amp = A
  lazy val amps = A
  lazy val ampere = A
  lazy val amperes = A

  lazy val K = units("K")
  lazy val kelvin = K

  lazy val cup = units("cup")
  lazy val cups = units("cups")

  lazy val tsp = units("tsp")
  lazy val teaspoon = tsp
  lazy val teaspoons = tsp

  lazy val tbsp = units("tbsp")
  lazy val tablespoon = tbsp
  lazy val tablespoons = tbsp

  lazy val oz = units("oz")
  lazy val ounce = oz
  lazy val ounces = oz

  lazy val ft2 = units("ft2")
  lazy val ft3 = units("ft3")
  lazy val sqft = ft2
  lazy val cuft = ft3

  lazy val btu = units("btu")
  lazy val btus = btu

  lazy val W = units("W")
  lazy val kW = units("kW")
  lazy val MW = units("MW")
  lazy val kWh = units("kWh")
  lazy val watt = W
  lazy val watts = W

  lazy val J = units("J")
  lazy val kcal = units("kcal")
  lazy val Cal = kcal
  lazy val joule = J
  lazy val joules = J

  lazy val mph = miles per hour
  lazy val kph = km per hour

  lazy val mol = units("mol")
  lazy val mole = units("mole")
  lazy val V = units("V")
  lazy val volt = V
  lazy val volts = V

  lazy val gal = units("gal")
  lazy val gallon = gal
  lazy val gallons = gal

  lazy val quart = units("quart")
  lazy val quarts = units("quarts")
  lazy val pint = units("pint")
  lazy val pints = units("pints")

  lazy val mpg = miles per gallon

  lazy val Hz = units("Hz")
  lazy val kHz = units("kHz")
  lazy val MHz = units("MHz")
  lazy val GHz = units("GHz")
  lazy val hertz = Hz

  lazy val bit = units("bit")
  lazy val bits = units("bits")

  lazy val byte = units("byte")
  lazy val bytes = units("bytes")

  lazy val kB = units("kB")
  lazy val MB = units("MB")
  lazy val GB = units("GB")
  lazy val TB = units("TB")
  lazy val PB = units("PB")

  lazy val KiB = units("KiB")
  lazy val MiB = units("MiB")
  lazy val GiB = units("GiB")
  lazy val TiB = units("TiB")
  lazy val PiB = units("PiB")
  
  lazy val bps = units("bps")
  lazy val kbps = units("kbps")
  lazy val Mbps = units("Mbps")
  lazy val Gbps = units("Gbps")
  lazy val Tbps = units("Tbps")
  lazy val Pbps = units("Pbps")
  lazy val Kibps = units("Kibps")
  lazy val Mibps = units("Mibps")
  lazy val Gibps = units("Gibps")
  lazy val Tibps = units("Tibps")
  lazy val Pibps = units("Pibps")

  lazy val USD = units("US$")

  lazy val dollar = units("dollar")
  lazy val dollars = units("dollars")

  private lazy val light = units("light")

  /**
   * Create units of length like light-{time}.
   * Example: <code>val ly = light(year)</code>
   * @param t Units of time
   * @return Units of length equivalent to the distance light travels in time
   *   <code>t</code>.
   */
  def light(t: Units): Units = {
    require(t is time, "Required time but got %s".format(t))
    light * t
  }

  // Need separate definitions for lightyears because a light year
  // is based on the Julian year (365.25 days), whereas in GNU units
  // a "year" is the tropical year (365.242198781 days).
  lazy val lightyears = units("lightyears")
  lazy val lightyear = units("lightyear")
  lazy val ly = units("ly")

  lazy val parsecs = units("parsecs")
  lazy val parsec = units("parsec")
  lazy val pc = units("pc")

  //----------------------------------------------------------------
  // Some common compound units.
  //
  // Example usage:
  //   val t = 3.28 (hours) in hms
  //   => List(3 hour, 16 min, 48 s)
  //

  lazy val hms = Seq(h, min, s)
  lazy val duration = Seq(year, day, h, min, s)
  lazy val dms = Seq(deg, arcmin, arcsec)
  lazy val ftin = Seq(ft, in)
  lazy val lboz = Seq(lbs, oz)

}
