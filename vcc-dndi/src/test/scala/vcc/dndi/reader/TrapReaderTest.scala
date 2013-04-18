/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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

import org.specs2.mutable.SpecificationWithJUnit
import vcc.infra.text._
import vcc.dndi.reader.Parser.BlockElement
import org.exnebula.iteratee.{Done, Empty, Chunk}
import scala.xml.Node

class TrapReaderTest extends SpecificationWithJUnit {

  "TrapSectionReader" should {

    "read attack section" in {
      val xmlChunks = List(
        <SPAN class="trapblocktitle">Attack</SPAN>,
        <SPAN class="trapblockbody"><B>Immediate Reaction</B> <B>Melee</B> <BR></BR></SPAN>,
        <SPAN class="trapblockbody"><B>Target: </B>The poor creature.</SPAN>,
        <SPAN class="trapblockbody"><B>Attack: </B>+8 vs. Reflex</SPAN>,
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>))

      val tbs = new TokenStream[BlockElement](xmlChunks.map(p=> Parser.parseBlockElement(p)))
      tbs.advance()

      val tr = new TrapReader(0)
      val sec = tr.processSection(tbs)
      (sec must not beNull)
      sec must_== TrapSection("Attack", StyledText(List(
        TextBlock("SPAN", "trapblockbody", TextSegment.makeBold("Immediate Reaction"), TextSegment(" "),
          TextSegment.makeBold("Melee"), TextSegment(" "), LineBreak),
          TextBlock("SPAN", "trapblockbody", TextSegment.makeBold("Target: "), TextSegment("The poor creature.")),
          TextBlock("SPAN","trapblockbody",List(TextSegment.makeBold("Attack: "), TextSegment("+8 vs. Reflex")))
      )))
    }

    "read attack section -iteratee" in {
      val xmlChunks = parseChunks(
        <SPAN class="trapblocktitle">Attack</SPAN>,
        <SPAN class="trapblockbody"><B>Immediate Reaction</B> <B>Melee</B> <BR></BR></SPAN>,
        <SPAN class="trapblockbody"><B>Target: </B>The poor creature.</SPAN>,
        <SPAN class="trapblockbody"><B>Attack: </B>+8 vs. Reflex</SPAN>,
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>))

      val tr = new TrapReader(0)
      val name = tr.readTrapBlockTitle.consume(Chunk(xmlChunks(0)))
      name must_== Done("Attack", Empty)

      val segment = tr.readTrapBlockBody.consume(Chunk(xmlChunks(1)))
      segment must_== Done(TextBlock("SPAN", "trapblockbody", TextSegment.makeBold("Immediate Reaction"), TextSegment(" "),
        TextSegment.makeBold("Melee"), TextSegment(" "), LineBreak)
        , Empty)

      val expected = TrapSection("Attack", StyledText(List(
        TextBlock("SPAN", "trapblockbody", TextSegment.makeBold("Immediate Reaction"), TextSegment(" "),
          TextSegment.makeBold("Melee"), TextSegment(" "), LineBreak),
          TextBlock("SPAN", "trapblockbody", TextSegment.makeBold("Target: "), TextSegment("The poor creature.")),
          TextBlock("SPAN","trapblockbody",List(TextSegment.makeBold("Attack: "), TextSegment("+8 vs. Reflex")))
      )))
      tr.readSection.consumeAll(xmlChunks) must_== (Right(expected),xmlChunks.drop(4))
    }

    "read section with Image ending with a P" in {
      val xmlChunks = List(
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>),
        (<SPAN class="trapblockbody"><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 5: Sneaky.<BR></BR><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 20: Almost.<BR></BR></SPAN>),
        (<P class="publishedIn">Published in <A target="_new" href="http://site.com">Lorem Lipsum</A>.</P>))

      val tbs = new TokenStream[BlockElement](xmlChunks.map(p=> Parser.parseBlockElement(p)))
      tbs.advance()

      val tr = new TrapReader(0)
      val sec = tr.processSection(tbs)
      (sec must not beNull)
      sec must_== TrapSection("Countermeasures", StyledText(List(
        TextBlock("SPAN", "trapblockbody",
          InlineImage("bullet.gif"), TextSegment(" Thievery DC 5: Sneaky."), LineBreak,
          InlineImage("bullet.gif"), TextSegment(" Thievery DC 20: Almost."), LineBreak)
        )))
    }

    "read section with Image ending with a P- iter" in {
      val xmlChunks = parseChunks(
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>),
        (<SPAN class="trapblockbody"><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 5: Sneaky.<BR></BR><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 20: Almost.<BR></BR></SPAN>),
        (<P class="publishedIn">Published in <A target="_new" href="http://site.com">Lorem Lipsum</A>.</P>))

      val tr = new TrapReader(0)
      val expected = TrapSection("Countermeasures", StyledText(List(
        TextBlock("SPAN", "trapblockbody",
          InlineImage("bullet.gif"), TextSegment(" Thievery DC 5: Sneaky."), LineBreak,
          InlineImage("bullet.gif"), TextSegment(" Thievery DC 20: Almost."), LineBreak)
      )))
      tr.readSection.consumeAll(xmlChunks) must_== (Right(expected),xmlChunks.drop(2))
    }

    "handle complete header information" in {
      val xmlChunks = List(
        (<H1 class="trap" xmlns="http://www.w3.org/1999/xhtml">Razor Spores (Elite)<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level 1 Elite Lurker<BR></BR><SPAN class="xp">XP 200</SPAN></SPAN></H1>),
        (<P>Footer</P>))

      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks.map(p => Parser.parseBlockElement(p)))

      trap("base:name") must_== Some("Razor Spores (Elite)")
      trap("base:level") must_== Some("1")
      trap("base:xp") must_== Some("200")
      trap("base:role") must_== Some("Elite Lurker")
      trap("base:type") must_== Some("Hazard")
    }

    "handle complete header information - iter" in {
      val xmlChunks = parseChunks(
        (<H1 class="trap" xmlns="http://www.w3.org/1999/xhtml">Razor Spores (Elite)<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level 1 Elite Lurker<BR></BR><SPAN class="xp">XP 200</SPAN></SPAN></H1>),
        (<P>Footer</P>))

      val tr = new TrapReader(0)
      val result = tr.readHeader.consumeAll(xmlChunks)
      result._1 must beRight
      result._2 must_== xmlChunks.drop(1)
      result._1.right.get must havePairs("name" -> "Razor Spores (Elite)", "role" -> "Elite Lurker", "xp" -> "200", "type" -> "Hazard", "level" -> "1")
    }

    "handle partial head 1" in {
      val xmlChunks = List(
        (<H1 class="trap" xmlns="http://www.w3.org/1999/xhtml">Angry Crowd<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level Party's level <BR></BR><SPAN class="xp">XP varies</SPAN></SPAN></H1>),
        (<P>Footer</P>))

      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks.map(p => Parser.parseBlockElement(p)))

      trap("base:name") must_== Some("Angry Crowd")
      trap("base:level") must_== Some("1")
      trap("base:xp") must_== Some("100")
      trap("base:role") must_== Some("Unspecified")
      trap("base:type") must_== Some("Hazard")
    }

    "handle partial head 2" in {
      val xmlChunks = List(
        (<H1 class="trap">Spiked Swinging Gate<BR></BR><SPAN class="type">Trap</SPAN><BR></BR><SPAN class="level">Level <BR></BR><SPAN class="xp">XP </SPAN></SPAN></H1>),
        (<P>Footer</P>))

      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks.map(p => Parser.parseBlockElement(p)))

      trap("base:name") must_== Some("Spiked Swinging Gate")
      trap("base:level") must_== Some("1")
      trap("base:xp") must_== Some("100")
      trap("base:role") must_== Some("Unspecified")
      trap("base:type") must_== Some("Trap")
    }

    "handle flavor line" in {
      val xmlChunks = List(
        (<H1 class="trap">Razor Spores (Elite)<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level 1 Elite Lurker<BR></BR><SPAN class="xp">XP 200</SPAN></SPAN></H1>),
        (<P class="flavor"><I>Glowing niceness.</I></P>),
        (<P>Footer</P>))

      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks.map(p => Parser.parseBlockElement(p)))

      trap.sections must_== List(TrapSection(null, StyledText(List(TextBlock("P","flavor",TextSegment.makeItalic("Glowing niceness."))))))
    }

    "handle flavor line - iter" in {
      val xmlChunks = parseChunks(
        (<P class="flavor"><I>Glowing niceness.</I></P>),
        (<P>Footer</P>))

      val tr = new TrapReader(0)
      val expected = TrapSection(null, StyledText(List(TextBlock("P","flavor",TextSegment.makeItalic("Glowing niceness.")))))
      tr.readFlavor.consumeAll(xmlChunks) must_== (Right(expected), xmlChunks.drop(1))
    }

    "handle initiative line" in {
      val xmlChunks = List(
        (<H1 class="trap">Razor Spores (Elite)<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level 1 Elite Lurker<BR></BR><SPAN class="xp">XP 200</SPAN></SPAN></H1>),
        (<SPAN class="traplead"><B>Initiative</B> +5</SPAN>),
        (<P>Footer</P>))

      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks.map(p => Parser.parseBlockElement(p)))

      trap.sections must_== List(TrapSection(null, StyledText(List(TextBlock("SPAN","traplead",TextSegment.makeBold("Initiative"),TextSegment(" +5"))))))
      trap("stat:initiative") must_== Some("5")
    }

    "handle initiative line - iter " in {
      val xmlChunks = parseChunks(
        (<SPAN class="traplead"><B>Initiative</B> +5</SPAN>),
        (<P>Footer</P>))

      val tr = new TrapReader(0)

      val expected = ("+5", TrapSection(null, StyledText(List(TextBlock("SPAN","traplead",TextSegment.makeBold("Initiative"),TextSegment(" +5"))))))
      tr.readInitiative.consumeAll(xmlChunks) must_== (Right(expected), xmlChunks.drop(1))
    }

    "handle description line" in {
      val xmlChunks = List(
        (<H1 class="trap">Razor Spores (Elite)<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level 1 Elite Lurker<BR></BR><SPAN class="xp">XP 200</SPAN></SPAN></H1>),
        (<SPAN class="traplead"><B>Hazard:</B> Something pops.</SPAN>),
        (<P>Footer</P>))

      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks.map(p => Parser.parseBlockElement(p)))

      trap.sections must_== List(TrapSection(null, StyledText(List(TextBlock("SPAN","traplead",TextSegment.makeBold("Hazard:"),TextSegment(" Something pops."))))))
    }

    "handle description line - iter" in {
      val xmlChunks = parseChunks(
        (<SPAN class="traplead"><B>Hazard:</B> Something pops.</SPAN>),
        (<P>Footer</P>))

      val tr = new TrapReader(0)

      val expected = TrapSection(null, StyledText(List(TextBlock("SPAN","traplead",TextSegment.makeBold("Hazard:"),TextSegment(" Something pops.")))))
      tr.readDescription.consumeAll(xmlChunks) must_== (Right(expected), xmlChunks.drop(1))
    }

    "handle comment line " in {
      val xmlChunks = List(
        (<H1 class="trap">Razor Spores (Elite)<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level 1 Elite Lurker<BR></BR><SPAN class="xp">XP 200</SPAN></SPAN></H1>),
        (<P>Published in <A target="_new" href="http://www.wizards.com/default.asp?x=products/dndacc/9780786950171">Seekers of the Ashen Crown</A>.</P>))

      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks.map(p => Parser.parseBlockElement(p)))

      trap("text:comment") must_== Some("Published in Seekers of the Ashen Crown .")
    }

    "handle blank trapblocktitle" in {
      val xmlChunks = List(
        (<H1 class="trap">Lake of Dreams<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level 1 Elite Lurker<BR></BR><SPAN class="xp">XP 200</SPAN></SPAN></H1>),
        (<SPAN class="trapblocktitle"></SPAN>),
        (<P>Bad lake,bad!</P>))

      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks.map(p => Parser.parseBlockElement(p)))

      trap("text:comment") must_== Some("Bad lake,bad!")
      trap.sections.length must_== 0
    }

    "handle blank trapblocktitle - iter" in {
      val xmlChunks = parseChunks(
        (<SPAN class="trapblocktitle"></SPAN>),
        (<SPAN class="trapblocktitle">Attack</SPAN>),
        (<SPAN class="trapblockbody"><B>Echo: </B>Charlie</SPAN>),
        (<SPAN class="trapblocktitle"></SPAN>),
        (<SPAN class="trapblocktitle"></SPAN>),
        (<SPAN class="trapblocktitle">Lorem</SPAN>),
        (<SPAN class="trapblockbody"><B>Echo: </B>Charlie</SPAN>),
        (<SPAN class="trapblocktitle"></SPAN>),
        (<P>Bad lake,bad!</P>))

      val tr = new TrapReader(0)
      val result = tr.readSections.consumeAll(xmlChunks)
      result._1 must beRight
      val sections = result._1.right.get
      extractSectionNames(sections) must_== List("Attack", "Lorem")
    }

    "handle blank trapblocktitle - iter" in {
      val xmlChunks = parseChunks(
        (<SPAN class="trapblocktitle"></SPAN>),
        (<P>Bad lake,bad!</P>))

      val tr = new TrapReader(0)
      val r = tr.readSections.consumeAll(xmlChunks)
      if(r._1.isLeft) r._1.left.get.printStackTrace()
      r must_== (Right(Nil), xmlChunks.drop(1))
    }

    "all together for - iter" in {
      val xmlChunks = parseChunks(
        (<H1 class="trap">Razor Spores (Elite)<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level 1 Elite Lurker<BR></BR><SPAN class="xp">XP 200</SPAN></SPAN></H1>),
        (<P class="flavor"><I>Glowing niceness.</I></P>),
        (<SPAN class="traplead"><B>Hazard:</B> Something pops.</SPAN>),
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>),
        (<SPAN class="trapblockbody"><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 5: Sneaky.<BR></BR><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 20: Almost.<BR></BR></SPAN>),
        (<SPAN class="traplead"><B>Initiative</B> +5</SPAN>),
        (<SPAN class="trapblocktitle">Attack</SPAN>),
        (<SPAN class="trapblockbody"><B>Immediate Reaction</B> <B>Melee</B> <BR></BR></SPAN>),
        (<SPAN class="trapblockbody"><B>Target: </B>The poor creature.</SPAN>),
        (<SPAN class="trapblockbody"><B>Attack: </B>+8 vs. Reflex</SPAN>),
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>),
        (<SPAN class="trapblockbody"><IMG src="images/bullet.gif" alt=""/>Pray.<BR/></SPAN>),
        (<P>Published in <A target="_new" href="http://site.com">Lorem Lipsum</A>.</P>))
      val tr = new TrapReader(0)

      val result = tr.readTrapOld.consumeAll(xmlChunks)

      result._1 must beRight
      result._2 must_== Nil
//      result._1.right.get must_== null
    }

    "all together without initiative - iter" in {
      val xmlChunks = parseChunks(
        (<H1 class="trap">Razor Spores (Elite)<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level 1 Elite Lurker<BR></BR><SPAN class="xp">XP 200</SPAN></SPAN></H1>),
        (<P class="flavor"><I>Glowing niceness.</I></P>),
        (<SPAN class="traplead"><B>Hazard:</B> Something pops.</SPAN>),
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>),
        (<SPAN class="trapblockbody"><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 5: Sneaky.<BR></BR><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 20: Almost.<BR></BR></SPAN>),
        (<SPAN class="trapblocktitle">Attack</SPAN>),
        (<SPAN class="trapblockbody"><B>Immediate Reaction</B> <B>Melee</B> <BR></BR></SPAN>),
        (<SPAN class="trapblockbody"><B>Target: </B>The poor creature.</SPAN>),
        (<SPAN class="trapblockbody"><B>Attack: </B>+8 vs. Reflex</SPAN>),
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>),
        (<SPAN class="trapblockbody"><IMG src="images/bullet.gif" alt=""/>Pray.<BR/></SPAN>),
        (<P class="publishedIn">Published in <A target="_new" href="http://site.com">Lorem Lipsum</A>.</P>))
      val tr = new TrapReader(0)

      val result = tr.readTrapOld.consumeAll(xmlChunks)

      result._1 must beRight
      result._2 must_== Nil
    }
  }

  def extractSectionNames(sections: List[TrapSection]): List[String] = {
    sections.map(x => x.header)
  }

  private def parseChunks(xmlChunks: Node*):List[BlockElement] = xmlChunks.map(Parser.parseBlockElement).toList
}