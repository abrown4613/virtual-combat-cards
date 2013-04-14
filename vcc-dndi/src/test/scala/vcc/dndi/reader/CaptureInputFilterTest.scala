/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
package vcc.dndi.reader

import org.specs2.SpecificationWithJUnit
import java.io.ByteArrayInputStream

class CaptureInputFilterTest extends SpecificationWithJUnit {
  def is =
    "convert UTF of nbsp" ! convert("a\u00a0b", "a b") ^
    "squash spaced including " ! convert("a\u00a0  . \r \n \u00a0b", "a . b") ^
    "uppercase for tags" ! convert("""<a href="abc">line<br/>break</a>""", """<A href="abc">line<BR/>break</A>""") ^
    "remove broken attribute" ! convert("""<a \="">""", """<A>""") ^
    end

  private def convert(input: String, expectedOutput: String) = {
    val filtered = CaptureInputFilter.filterInput(new ByteArrayInputStream(input.getBytes("UTF-8")))
    filtered must_== expectedOutput
  }
}