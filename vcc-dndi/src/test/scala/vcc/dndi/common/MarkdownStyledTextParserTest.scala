/*
 * Copyright (C) 2013-2013 - Thomas Santana <tms@exnebula.org>
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

import org.specs2.SpecificationWithJUnit
import vcc.infra.text._

class MarkdownStyledTextParserTest extends SpecificationWithJUnit {
  def is =
    "MarkdownStyledTextParser".title ^
      cases ^
      end

  val cases = Seq(
    c("handle simple text", "[]some text", StyledText.singleBlock("P", null, "some text")),
    c("handle text with tagClass", "[class] some text", StyledText.singleBlock("P", "class", " some text")),
    c("handle text with tag and tagClass", "[SPAN:class] some text",
      StyledText.singleBlock("SPAN", "class", " some text")),
    c("handle line break in single block", "[class] some text\n more text",
      StyledText(List(TextBlock("P", "class", TextSegment(" some text"), LineBreak, TextSegment(" more text"))))),
    c("handle last line break as break in last block", "[class] some text\n more text\n",
      StyledText(List(TextBlock("P", "class", TextSegment(" some text"), LineBreak, TextSegment(" more text"), LineBreak)))),
    c("handle two blocks", "[class] some text\n[class2] more text",
      StyledText(List(TextBlock("P", "class", TextSegment(" some text")), TextBlock("P", "class2", TextSegment(" more text"))))),
    c("handle bold", "[class] some *bold*",
      StyledText(List(TextBlock("P", "class", TextSegment(" some "), TextSegment.makeBold("bold"))))),
    c("handle italic", "[class] some _italic_",
      StyledText(List(TextBlock("P", "class", TextSegment(" some "), TextSegment.makeItalic("italic"))))),
    c("handle icon", "[class] some {icon.gif}",
      StyledText(List(TextBlock("P", "class", TextSegment(" some "), InlineImage("icon.gif"))))),
    c("handle two blocks and breaks", "[class] some text\ncomplement\n[class2] more text",
      StyledText(List(
        TextBlock("P", "class", TextSegment(" some text"), LineBreak, TextSegment("complement")),
        TextBlock("P", "class2", TextSegment(" more text"))))),
    c("all together", "[class] some _text_\nwith *bold*\n[class2] more {icon.gif}* text*",
      StyledText(List(
        TextBlock("P", "class", TextSegment(" some "), TextSegment.makeItalic("text"), LineBreak, TextSegment("with "), TextSegment.makeBold("bold")),
        TextBlock("P", "class2", TextSegment(" more "), InlineImage("icon.gif"), TextSegment.makeBold(" text")))))
  )

  private def c(description: String, source: String, expected: StyledText) = {
    description ! {
      f(source) must_== Right(expected)
    }
  }

  private def f(source: String) = MarkdownStyledTextParser.parse(source)
}