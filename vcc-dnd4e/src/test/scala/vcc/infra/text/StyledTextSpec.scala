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
package vcc.infra.text

import org.specs.{SpecificationWithJUnit}

class StyledTextSpec extends SpecificationWithJUnit {
  "TextBuilder" should {

    "add block level elements" in {
      val builder = new TextBuilder()

      builder.append(Block("P", "first", TextSegment(Style.Bold, "Hello"), TextSegment("darling")))
      builder.getDocument() must_== StyledText(List(Block("P", "first", List(TextSegment(Style.Bold, "Hello"), TextSegment(Style.None, "darling")))))
    }

    "append new block to the existing document" in {
      val builder = new TextBuilder()

      builder.append(Block("P", "first", TextSegment(Style.Bold, "Hello"), TextSegment("darling")))
      builder.append(Block("P", "second", TextSegment(Style.Italic, "you're so fine")))
      builder.getDocument() must_== StyledText(List(
        Block("P", "first", List(TextSegment(Style.Bold, "Hello"), TextSegment(Style.None, "darling"))),
        Block("P", "second", List(TextSegment(Style.Italic, "you're so fine")))
        ))
    }
  }

  "StyledText serialization" should {
    val text = StyledText(List(
      Block("P", "first", List(TextSegment(Style.Bold, "Hello"), TextSegment(Style.None, "darling"), LineBreak, TextSegment("love"))),
      Block("P", "second", List(TextSegment(Style.Italic, "you're so fine")))
      ))

    "serialize TextSegment" in {
      TextSegment(Style.Bold, "Hello").toXML() must_== <text style="Bold">Hello</text>
    }

    "deserialize TextSegment" in {
      Block.extractSegment(<text style="Bold">Hello</text>) must_== TextSegment(Style.Bold,"Hello")
      Block.extractSegment(<text style="None">Hello</text>) must_== TextSegment(Style.None,"Hello")
      Block.extractSegment(<text style="Italic">Hello</text>) must_== TextSegment(Style.Italic,"Hello")
    }
    
    "serialize Block" in {
      Block("DIV", "blast").toXML must_== <block tag="DIV" class="blast"></block>
    }

    "serialize Break" in {
      LineBreak.toXML must_== <break/>
    }

    "serialize to XML" in {
      text.toXML() must_== <styledText><block tag="P" class="first"><text style="Bold">Hello</text><text style="None">darling</text><break/><text style="None">love</text></block><block tag="P" class="second"><text style="Italic">you're so fine</text></block></styledText>
    }

    "deserialize from XML" in {
      val xml = <styledText>
        <block tag="P" class="first">
          <text style="Bold">Hello</text> <text style="None">darling</text> <break/> <text style="None">love</text>
        </block> <block tag="P" class="second">
          <text style="Italic">you're so fine</text>
        </block>
      </styledText>
      StyledText.fromXML(xml) must_== text 
    }
  }

  "StyledText XHTML conversion" should {
    val blk1 = Block("P", "first", List(TextSegment(Style.Bold, "Hello"), TextSegment(Style.None, "darling"), LineBreak, TextSegment("love")))
    val blk2 = Block("P", "second", List(TextSegment(Style.Italic, "you're so fine")))

    "produce block level entities" in {
      blk1.toXHTML must_== <P class="first"><b>Hello</b>darling<br/>love</P>
      blk2.toXHTML must_== <P class="second"><i>you're so fine</i></P>
    }
    "produce sequence of nodes" in {
      val fmt = StyledText(List(blk1, blk2)).toXHTML

      fmt.size must_== 2
      fmt(0) must_== blk1.toXHTML
      fmt(1) must_== blk2.toXHTML
    }
  }
}