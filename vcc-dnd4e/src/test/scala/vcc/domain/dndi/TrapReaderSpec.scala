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
import vcc.infra.text._
import vcc.domain.dndi.Parser.{BlockElement} 

@RunWith(classOf[JUnitSuiteRunner])
class TrapReaderTest extends JUnit4(TrapReaderSpec)

object TrapReaderSpec extends Specification {

  "TrapSectionReader" should {

    "read attack section" in {
      val xmlChunks = List(
        <SPAN class="trapblocktitle">Attack</SPAN>,
        <SPAN class="trapblockbody"><B>Immediate Reaction</B> <B>Melee</B> <BR></BR></SPAN>,
        <SPAN class="trapblockbody"><B>Target: </B>The poor creature.</SPAN>,
        <SPAN class="trapblockbody"><B>Attack: </B>+8 vs. Reflex</SPAN>,
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>))

      val tbs = new TokenStream[BlockElement](xmlChunks.map(p=> Parser.parseBlockElement(p, true)))
      tbs.advance()

      val tr = new TrapReader(0)
      val sec = tr.processSection(tbs)
      sec must notBeNull
      sec must_== TrapSection("Attack", StyledText(List(
        TextBlock("SPAN", "trapblockbody", TextSegment.makeBold("Immediate Reaction"), TextSegment(" "),
          TextSegment.makeBold("Melee"), TextSegment(" "), LineBreak),
          TextBlock("SPAN", "trapblockbody", TextSegment.makeBold("Target: "), TextSegment("The poor creature.")),
          TextBlock("SPAN","trapblockbody",List(TextSegment.makeBold("Attack: "), TextSegment("+8 vs. Reflex")))
      )))
    }

    "read section with Image ending with a P" in {
      val xmlChunks = List(
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>),
        (<SPAN class="trapblockbody"><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 5: Sneaky.<BR></BR><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 20: Almost.<BR></BR></SPAN>),
        (<P xmlns="http://www.w3.org/1999/xhtml">Published in <A target="_new" href="http://www.wizards.com/default.asp?x=products/dndacc/217647400">FR1 Scepter Tower of Spellgard</A>.</P>)
      )

      val tbs = new TokenStream[BlockElement](xmlChunks.map(p=> Parser.parseBlockElement(p, true)))
      tbs.advance()

      val tr = new TrapReader(0)
      val sec = tr.processSection(tbs)
      sec must notBeNull
      sec must_== TrapSection("Countermeasures", StyledText(List(
        TextBlock("SPAN", "trapblockbody",
          InlineImage("bullet.gif"), TextSegment(" Thievery DC 5: Sneaky."), LineBreak,
          InlineImage("bullet.gif"), TextSegment(" Thievery DC 20: Almost."), LineBreak)
        )))
    }

    "handle complete header information" in {
      val xmlChunks = List(
        (<H1 class="trap" xmlns="http://www.w3.org/1999/xhtml">Razor Spores (Elite)<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level 1 Elite Lurker<BR></BR><SPAN class="xp">XP 200</SPAN></SPAN></H1>),
        (<P>Footer</P>))

      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks.map(p => Parser.parseBlockElement(p, true)))

      trap("base:name") must_== Some("Razor Spores (Elite)")
      trap("base:level") must_== Some("1")
      trap("base:xp") must_== Some("200")
      trap("base:role") must_== Some("Elite Lurker")
      trap("base:type") must_== Some("Hazard")
    }

    "handle partial head 1" in {
      val xmlChunks = List(
        (<H1 class="trap" xmlns="http://www.w3.org/1999/xhtml">Angry Crowd<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level Party's level <BR></BR><SPAN class="xp">XP varies</SPAN></SPAN></H1>),
        (<P>Footer</P>))

      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks.map(p => Parser.parseBlockElement(p, true)))

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
      val trap = tr.process(xmlChunks.map(p => Parser.parseBlockElement(p, true)))

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
      val trap = tr.process(xmlChunks.map(p => Parser.parseBlockElement(p, true)))

      trap.sections must_== List(TrapSection(null, StyledText(List(TextBlock("P","flavor",TextSegment.makeItalic("Glowing niceness."))))))    
    }

    "handle initiative line" in {
      val xmlChunks = List(
        (<H1 class="trap">Razor Spores (Elite)<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level 1 Elite Lurker<BR></BR><SPAN class="xp">XP 200</SPAN></SPAN></H1>),
        (<SPAN class="traplead"><B>Initiative</B> +5</SPAN>),
        (<P>Footer</P>))

      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks.map(p => Parser.parseBlockElement(p, true)))

      trap.sections must_== List(TrapSection(null, StyledText(List(TextBlock("SPAN","traplead",TextSegment.makeBold("Initiative"),TextSegment(" +5"))))))
      trap("stat:initiative") must_== Some("5")
    }

    "handle description line" in {
      val xmlChunks = List(
        (<H1 class="trap">Razor Spores (Elite)<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level 1 Elite Lurker<BR></BR><SPAN class="xp">XP 200</SPAN></SPAN></H1>),
        (<SPAN class="traplead"><B>Hazard:</B> Something pops.</SPAN>),
        (<P>Footer</P>))

      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks.map(p => Parser.parseBlockElement(p, true)))

      trap.sections must_== List(TrapSection(null, StyledText(List(TextBlock("SPAN","traplead",TextSegment.makeBold("Hazard:"),TextSegment(" Something pops."))))))
    }
  }
}