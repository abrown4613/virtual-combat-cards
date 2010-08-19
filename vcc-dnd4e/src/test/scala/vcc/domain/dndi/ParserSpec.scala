/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
//$Id$
package vcc.domain.dndi 


import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4,JUnitSuiteRunner}
import Parser._

@RunWith(classOf[JUnitSuiteRunner])
class ParserTest extends JUnit4(ParserSpec)

object ParserSpec extends Specification {
  "Parser.parseNode" should {
    "parse <B>text<BR/>line<B>" in {
      Parser.parseNode(<B>text<BR/>line</B>) must_== Key("text\nline")
    }

    "parser <B></B>" in {
      Parser.parseNode(<B></B>) must_== Key("")
    }
    "parser <B><IMG/></B>" in {
      //Some really bad HTML here
      Parser.parseNode(<B><IMG/></B>) must_== Key("")
    }
  }
}