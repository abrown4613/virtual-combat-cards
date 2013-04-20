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
package vcc.dndi.common

import org.specs2.mutable.SpecificationWithJUnit
import vcc.advtools.FormattedTextParser
import vcc.dndi.common.FormattedText._

class FormattedTextParserTest extends SpecificationWithJUnit {

  "formatted text parser" should {
    "read markdown italic" in {
      val r = FormattedTextParser.parseBlock("_Italic_Not Italic\n")
      r.get must_== Block(Seq(Line(0, Seq(Italic("Italic"), Normal("Not Italic")))))
    }
    "read markdown bold" in {
      val r = FormattedTextParser.parseBlock("*Bold*Not bold\n")
      r.get must_== Block(Seq(Line(0, Seq(Bold("Bold"), Normal("Not bold")))))
    }
    "read markdown ident" in {
      val r = FormattedTextParser.parseBlock("\t\tSome _Italic_ and not italic\n")
      r.get must_== Block(Seq(Line(2, Seq(Normal("Some "), Italic("Italic"), Normal(" and not italic")))))
    }
    "read several lines ignoring line breaks" in {
      val r = FormattedTextParser.parseBlock("_Italic_Not Italic\n\n\n" + "\tSome _Italic_ and not italic\n\n")
      r.get must_== Block(Seq(
        Line(0, Seq(Italic("Italic"), Normal("Not Italic"))),
        Line(1, Seq(Normal("Some "), Italic("Italic"), Normal(" and not italic")))))
    }
    "handle image links" in {
      val r = FormattedTextParser.parseBlock("\t\t{icon.gif} has *image* too\n")
      r.get must_== Block(Seq(Line(2, Seq(Image("icon.gif"), Normal(" has "), Bold("image"), Normal(" too")))))
    }
    "handle image links translating links" in {
      val imageProcessor = (s:String) => s.substring(0, s.indexOf("."))
      val r = FormattedTextParser.parseBlock("\t\t{icon.gif} has *image* too\n", imageProcessor)
      imageProcessor("icon.gif") must_== "icon"
      r.get must_== Block(Seq(Line(2, Seq(Image("icon"), Normal(" has "), Bold("image"), Normal(" too")))))
    }
    "read several lines ignoring line breaks" in {
      FormattedTextParser.parseBlock("_Italic_Not \t Italic") must_== None
    }
    "formatted parsed result matches toString" in {
      val text = "\t*Bold* definition\n_Italic_ comment"
      FormattedTextParser.parseBlock(text).get.toString must_== """"\t*Bold* definition\n_Italic_ comment""""
    }
  }
}