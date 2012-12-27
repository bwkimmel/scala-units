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
 * Indicates an attempt to convert to Units of differing dimensions.
 * @param from The Units that were being converted.
 * @param to The Units to which conversion was requested.
 */
class IncompatibleUnitsException(from: Units, to: Units)
  extends IllegalArgumentException("Incompatible units: %s -> %s".format(from, to))

/**
 * Indicates a parsing error ocurred while attempting to parse units.
 * @param units The string being parsed.
 */
class UnitsParsingException(units: String)
  extends IllegalArgumentException("Unable to parse units: %s".format(units))

/**
 * Indicates a parsing error ocurred while attempting to parse a command.
 * @param cmd The string being parsed.
 */
class CommandParsingException(cmd: String)
  extends IllegalArgumentException("Unable to parse command: %s".format(cmd))

/**
 * Indicates the presence of an undefined unit while attempting to resolve
 * units.
 * @param symbol The symbol for the undefined unit.
 */
class UndefinedUnitsException(symbol: String)
  extends IllegalArgumentException("Cannot resolve symbol: %s".format(symbol))

