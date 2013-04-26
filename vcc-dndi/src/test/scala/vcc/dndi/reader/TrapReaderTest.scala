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

import org.specs2.mutable.SpecificationWithJUnit
import vcc.dndi.reader.Parser.{Text, NonBlock, BlockElement}
import org.exnebula.iteratee.{Done, Empty, Chunk}
import scala.xml.Node
import vcc.dndi.common.MarkdownStyledTextParser

class TrapReaderTest extends SpecificationWithJUnit {
  private val xmlHead = (<H1 class="trap" >Razor Spores (Elite)<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level 1 Elite Lurker<BR></BR><SPAN class="xp">XP 200</SPAN></SPAN></H1>)
  private val xmlComment = (<P>Footer</P>)
  private val xmlFlavor = (<P class="flavor"><I>Glowing niceness.</I></P>)
  private val xmlTrapLead = (<SPAN class="traplead"><B>Hazard:</B> Something pops.</SPAN>)
  private val sectionAttack = TrapSection("Attack",
    m("[thStat]*Immediate Reaction* *Melee* \n \n" +
      "[thStat]*Target: *The poor creature.\n" +
      "[thStat]*Attack: *+8 vs. Reflex"))

  "TrapSectionReader with old format" should {

    "read attack section" in {
      val xmlChunks = parseChunks(
        <SPAN class="trapblocktitle">Attack</SPAN>,
        <SPAN class="trapblockbody"><B>Immediate Reaction</B> <B>Melee</B> <BR></BR> </SPAN>,
        <SPAN class="trapblockbody"><B>Target: </B>The poor creature.</SPAN>,
        <SPAN class="trapblockbody"><B>Attack: </B>+8 vs. Reflex</SPAN>,
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>))

      val tr = new TrapReader(0)
      val name = tr.readTrapBlockTitle.consume(Chunk(xmlChunks(0)))
      name must_== Done("Attack", Empty)

      val segment = tr.readTrapBlockBody.consume(Chunk(xmlChunks(1)))
      segment must_== Done(m("[thStat]*Immediate Reaction* *Melee* \n ").blocks(0), Empty)

      tr.readSection.consumeAll(xmlChunks) must_== (Right(sectionAttack),xmlChunks.drop(4))
    }

    "read section with Image ending with a P" in {
      val xmlChunks = parseChunks(
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>),
        (<SPAN class="trapblockbody"><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 5: Sneaky.<BR></BR><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 20: Almost.<BR></BR></SPAN>),
        (<P class="publishedIn">Published in <A target="_new" href="http://site.com">Lorem Lipsum</A>.</P>))

      val tr = new TrapReader(0)
      val expected = TrapSection("Countermeasures", m(
        "[thStat]{bullet.gif} Thievery DC 5: Sneaky.\n{bullet.gif} Thievery DC 20: Almost.\n"))

      val result = tr.readSection.consumeAll(xmlChunks)
      result must_== (Right(expected), xmlChunks.drop(2))
    }

    "handle complete header information" in {
      val xmlChunks = parseChunks(xmlHead, xmlFlavor, xmlTrapLead, xmlComment)
      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks)

      trap("base:name") must_== Some("Razor Spores (Elite)")
      trap("base:level") must_== Some("1")
      trap("base:xp") must_== Some("200")
      trap("base:role") must_== Some("Elite Lurker")
      trap("base:type") must_== Some("Hazard")
    }

    "handle wrapping whitespace" in {
      val xmlWhiteSpace = NonBlock(List(Text("    ")))
      val xmlChunks = xmlWhiteSpace :: parseChunks(xmlHead, xmlFlavor, xmlTrapLead, xmlComment) ::: List(xmlWhiteSpace)
      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks)

      trap("base:name") must_== Some("Razor Spores (Elite)")
    }

    "handle complete header information" in {
      val xmlChunks = parseChunks(xmlHead, xmlFlavor, xmlTrapLead, xmlComment)

      val tr = new TrapReader(0)
      val result = tr.readHeader.consumeAll(xmlChunks)
      result._1 must beRight
      result._2 must_== xmlChunks.drop(1)
      result._1.right.get must havePairs("name" -> "Razor Spores (Elite)", "role" -> "Elite Lurker", "xp" -> "200", "type" -> "Hazard", "level" -> "1")
    }

    "handle partial head 1" in {
      val trap = readChunks(
        (<H1 class="trap" xmlns="http://www.w3.org/1999/xhtml">Angry Crowd<BR></BR><SPAN class="type">Hazard</SPAN><BR></BR><SPAN class="level">Level Party's level <BR></BR><SPAN class="xp">XP varies</SPAN></SPAN></H1>),
        xmlFlavor, xmlTrapLead, xmlComment)

      trap("base:name") must_== Some("Angry Crowd")
      trap("base:level") must_== Some("1")
      trap("base:xp") must_== Some("100")
      trap("base:role") must_== Some("Unspecified")
      trap("base:type") must_== Some("Hazard")
    }

    "handle partial head 2" in {
      val trap = readChunks(
        (<H1 class="trap">Spiked Swinging Gate<BR></BR><SPAN class="type">Trap</SPAN><BR></BR><SPAN class="level">Level <BR></BR><SPAN class="xp">XP </SPAN></SPAN></H1>),
        xmlFlavor, xmlTrapLead, xmlComment)

      trap("base:name") must_== Some("Spiked Swinging Gate")
      trap("base:level") must_== Some("1")
      trap("base:xp") must_== Some("100")
      trap("base:role") must_== Some("Unspecified")
      trap("base:type") must_== Some("Trap")
    }

    "handle flavor line" in {
      val trap = readChunks(xmlHead, xmlFlavor, xmlTrapLead, xmlComment)

      trap.sections must_== List(
        TrapSection(null, m("[flavor]_Glowing niceness._")),
        TrapSection(null, m("[thStat]*Hazard:* Something pops."))
      )
    }

    "handle trap without flavor line" in {
      val trap = readChunks(xmlHead, xmlTrapLead, xmlComment)

      trap.sections must_== List(
        TrapSection(null, m("[thStat]*Hazard:* Something pops."))
      )
    }

    "handle trap without first traplead line" in {
      val trap = readChunks(xmlHead, xmlFlavor, xmlComment)

      trap.sections must_== List(
        TrapSection(null, m("[flavor]_Glowing niceness._"))
      )
    }

    "handle flavor line" in {
      val xmlChunks = parseChunks(xmlFlavor, xmlComment)

      val tr = new TrapReader(0)
      val expected = TrapSection(null, m("[flavor]_Glowing niceness._"))
      tr.readFlavor.consumeAll(xmlChunks) must_== (Right(expected), xmlChunks.drop(1))
    }

    "handle initiative line" in {
      val trap = readChunks(xmlHead, xmlFlavor, xmlTrapLead,
        (<SPAN class="trapblocktitle">Detection</SPAN>),
        (<SPAN class="trapblockbody">Thievery DC 5: Sneaky.</SPAN>),
        (<SPAN class="traplead"><B>Initiative</B> +5</SPAN>), xmlComment)

      trap.sections.last must_== TrapSection(null, m("[thStat]*Initiative* +5"))
      trap("stat:initiative") must_== Some("5")
    }

    "handle initiative line" in {
      val xmlChunks = parseChunks(
        (<SPAN class="traplead"><B>Initiative</B> +11</SPAN>), xmlComment)

      val tr = new TrapReader(0)

      val expected = ("11", TrapSection(null, m("[thStat]*Initiative* +11")))
      tr.readInitiative.consumeAll(xmlChunks) must_== (Right(expected), xmlChunks.drop(1))
    }

    "handle description line" in {
      val xmlChunks = parseChunks(xmlHead, xmlFlavor, xmlTrapLead, xmlComment)

      val tr = new TrapReader(0)
      val trap = tr.process(xmlChunks)

      trap.sections.last must_== TrapSection(null, m("[thStat]*Hazard:* Something pops."))
    }

    "handle description line single" in {
      val xmlChunks = parseChunks(xmlTrapLead, xmlComment)
      val tr = new TrapReader(0)

      val expected = TrapSection(null, m("[thStat]*Hazard:* Something pops."))
      tr.readDescription.consumeAll(xmlChunks) must_== (Right(expected), xmlChunks.drop(1))
    }

    "handle comment line" in {
      val trap = readChunks(xmlHead, xmlFlavor, xmlTrapLead,
        (<P>Published in <A target="_new" href="http://www.wizards.com/default.asp?x=products/dndacc/9780786950171">Seekers of the Ashen Crown</A>.</P>))

      trap("text:comment") must_== Some("Published in Seekers of the Ashen Crown .")
    }

    "handle blank trapblocktitle" in {
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

    "handle blank trapblocktitle with no other sections" in {
      val xmlChunks = parseChunks(
        (<SPAN class="trapblocktitle"></SPAN>),
        (<P>Bad lake,bad!</P>))

      val tr = new TrapReader(0)
      val r = tr.readSections.consumeAll(xmlChunks)
      if(r._1.isLeft) r._1.left.get.printStackTrace()
      r must_== (Right(Nil), xmlChunks.drop(1))
    }

    "all together for with initiative" in {
      val xmlChunks = parseChunks(xmlHead, xmlFlavor, xmlTrapLead,
        (<SPAN class="trapblocktitle">Detection</SPAN>),
        (<SPAN class="trapblockbody"><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 5: Sneaky.<BR></BR><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 20: Almost.<BR></BR></SPAN>),
        (<SPAN class="traplead"><B>Initiative</B> +5</SPAN>),
        (<SPAN class="trapblocktitle">Attack</SPAN>),
        (<SPAN class="trapblockbody"><B>Immediate Reaction</B> <B>Melee</B> <BR></BR> </SPAN>),
        (<SPAN class="trapblockbody"><B>Target: </B>The poor creature.</SPAN>),
        (<SPAN class="trapblockbody"><B>Attack: </B>+8 vs. Reflex</SPAN>),
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>),
        (<SPAN class="trapblockbody"><IMG src="images/bullet.gif" alt=""/>Pray.<BR/></SPAN>),
        (<P>Published in <A target="_new" href="http://site.com">Lorem Lipsum</A>.</P>))

      val tr = new TrapReader(0)
      val result = tr.readTrapOld.consumeAll(xmlChunks)

      result._1 must beRight
      result._2 must_== Nil
      val sections = result._1.right.get.sections

      result._1.right.get("stat:initiative") must_== Some("5")
      sections(0) must_== TrapSection(null, m("[flavor]_Glowing niceness._"))
      sections(1) must_== TrapSection(null, m("[thStat]*Hazard:* Something pops."))
      sections(2) must_== TrapSection("Detection",
        m("[thStat]{bullet.gif} Thievery DC 5: Sneaky.\n{bullet.gif} Thievery DC 20: Almost.\n"))
      sections(3) must_== TrapSection(null, m("[thStat]*Initiative* +5"))
      sections(4) must_== sectionAttack
      sections(5) must_== TrapSection("Countermeasures", m("[thStat]{bullet.gif}Pray.\n"))
    }

    "all together without initiative no intiative" in {
      val xmlChunks = parseChunks(xmlHead, xmlFlavor, xmlTrapLead,
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>),
        (<SPAN class="trapblockbody"><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 5: Sneaky.<BR></BR><IMG src="images/bullet.gif" alt=""></IMG> Thievery DC 20: Almost.<BR></BR></SPAN>),
        (<SPAN class="trapblocktitle">Attack</SPAN>),
        (<SPAN class="trapblockbody"><B>Immediate Reaction</B> <B>Melee</B> <BR></BR> </SPAN>),
        (<SPAN class="trapblockbody"><B>Target: </B>The poor creature.</SPAN>),
        (<SPAN class="trapblockbody"><B>Attack: </B>+8 vs. Reflex</SPAN>),
        (<SPAN class="trapblocktitle">Countermeasures</SPAN>),
        (<SPAN class="trapblockbody"><IMG src="images/bullet.gif" alt=""/>Pray.<BR/></SPAN>),
        (<P class="publishedIn">Published in <A target="_new" href="http://site.com">Lorem Lipsum</A>.</P>))
      val tr = new TrapReader(0)
      val result = tr.readTrapOld.consumeAll(xmlChunks)

      result._1 must beRight
      result._1.right.get("stat:initiative") must_== Some("-99")
      result._1.right.get.sections(3) must_== sectionAttack
      result._2 must_== Nil
    }
  }

  /* Test for new trap format */
  private val newHeader = (<H1 class="thHead">Boomer<BR/><SPAN class="thSubHead">Object</SPAN><BR/><SPAN class="thLevel">Level 4 Elite Trap<SPAN class="thXP">XP 350</SPAN></SPAN></H1>)
  private val newComment = (<P class="publishedIn">Published in <A target="_new" href="http://www.wizards.com/">book</A>.</P>)
  private val newTrapAttack = Seq(
    (<H2 class="thHead">Triggered Actions</H2>),
    (<P class="th2"><IMG src="http://www.wizards.com/dnd/images/symbol/Z1a.gif"/><B>Attack</B> (fire) </P>),
    (<P class="tbod"><I>Trigger</I>: you enter area.</P>),
    (<P class="tbod"><I>Attack</I> (<I>No Action</I>): Close burst 2 (creatures in burst); +7 vs. Reflex</P>),
    (<P class="tbod"><I>Miss</I>: Half damage.</P>),
    (<P class="tbod"><I>Effect</I>: Lots of fire.</P>),
    (<P class="tbod"><I>Special</I>: More fire.</P>))
  private val sectionAttackNew = TrapSection("Triggered Actions",
    m("[th2]{z1a.gif}*Attack* (fire) \n" +
      "[tbod]_Trigger_: you enter area.\n" +
      "[tbod]_Attack_ (_No Action_): Close burst 2 (creatures in burst); +7 vs. Reflex\n" +
      "[tbod]_Miss_: Half damage.\n" +
      "[tbod]_Effect_: Lots of fire.\n" +
      "[tbod]_Special_: More fire."))

  private val newTraits = Seq(
    (<H2 class="thHead">Traits</H2>),
    (<P class="th2"><B>Green</B> </P>),
    (<P class="thStat">Tree like.</P>),
    (<P class="th2"><B>Round</B> </P>),
    (<P class="thStat">Ball like.</P>))

  private val sectionNewTraits = TrapSection("Traits",
    m("[th2]*Green* \n[thStat]Tree like.\n" +
      "[th2]*Round* \n[thStat]Ball like."))

  private val newStatNoInit = Seq(
    (<P class="thStat"><B>Detect</B> Perception DC 14</P>),
    (<SPAN class="thInit"><B>Initiative</B> —</SPAN>))

  private val sectionHeadNoInit = TrapSection(null, m("[thStat]*Detect* Perception DC 14\n[thInit]*Initiative* —"))

  private val newStatWithInit = Seq(
    (<P class="thStat"><B>Detect</B> automatic</P>),
    (<SPAN class="thInit"><B>Initiative</B> +19</SPAN>),
    (<P class="thStat"><B>Immune</B> attacks </P>))

  private val newStatWithAllStats = Seq(
    (<P class="thStat"><B>Detect</B> —</P>),
    (<SPAN class="thInit"><B>Initiative</B> -2</SPAN>),
    (<P class="thStat"><B>HP</B> 435</P>),
    (<P class="thStat"><B>AC</B> 31, <B>Fortitude</B> —, <B>Reflex</B> 29, <B>Will</B> 30</P>),
    (<P class="thStat"><B>Immune</B> attacks </P>))

  private val newCountermeasures = Seq(
    (<H2 class="thHead">Countermeasures</H2>),
    (<P class="thBody"><IMG src="symbol/X.gif"/> <B>Disable</B>: Thievery DC 21 (standard action). <I>Success:</I> The trap is disabled.<I> Failure (16 or lower):</I> The trap triggers.</P>))

  private val sectionCounterMeasures = TrapSection("Countermeasures",
    m("[thBody]{x.gif} *Disable*: Thievery DC 21 (standard action). _Success:_ The trap is disabled." +
      "_ Failure (16 or lower):_ The trap triggers."))

  "TrapSectionReader with new format" should {
    "handle header and comment" in {
      val trap = readChunks(newHeader, newComment)
      trap("base:name") must_== Some("Boomer")
      trap("base:level") must_== Some("4")
      trap("base:xp") must_== Some("350")
      trap("base:role") must_== Some("Elite")
      trap("base:type") must_== Some("Trap")
      trap("text:comment") must_== Some("Published in book .")
    }

    "handle header without role" in {
      val header = (<H1 class="thHead">Nice Trap<BR/><SPAN class="thSubHead">Object</SPAN><BR/><SPAN class="thLevel">Level 15 Hazard<SPAN class="thXP">XP 1200</SPAN></SPAN></H1>)
      val trap = readChunks(header, newComment)
      trap("base:name") must_== Some("Nice Trap")
      trap("base:level") must_== Some("15")
      trap("base:xp") must_== Some("1200")
      trap("base:role") must_== Some("Standard")
      trap("base:type") must_== Some("Hazard")
      trap("text:comment") must_== Some("Published in book .")
    }

    "trap without level or xp" in {
      val head = (<H1 class="thHead">Blob<BR/><SPAN class="thSubHead">Object</SPAN><BR/><SPAN class="thLevel">Level Varies Trap<SPAN class="thXP">XP Varies</SPAN></SPAN></H1>)
      val trap = readChunks(Seq(head) ++ newStatNoInit ++ Seq(newComment):_*)
      trap("base:name") must_== Some("Blob")
      trap("base:level") must_== Some("1")
      trap("base:xp") must_== Some("100")
      trap("base:type") must_== Some("Trap")
      trap("base:role") must_== Some("Standard")
      trap("text:comment") must_== Some("Published in book .")
     }

    "trap with initiative" in {
      val trap = readChunks(Seq(newHeader) ++ newStatWithInit ++ Seq(newComment):_*)
      trap.sections(0) must_== TrapSection(null,
        m("[thStat]*Detect* automatic\n[thInit]*Initiative* +19\n[thStat]*Immune* attacks "))
      trap("stat:initiative") must_== Some("19")
      trap("stat:hp") must_== Some("9999")
      trap("stat:ac") must_== Some("99")
    }

    "trap without initiative" in {
      val trap = readChunks(Seq(newHeader) ++ newStatNoInit ++ Seq(newComment):_*)
      trap.sections(0) must_== sectionHeadNoInit
      trap("stat:initiative") must_== Some("-99")
    }

    "trap with initiative more stats" in {
      val trap = readChunks(Seq(newHeader) ++ newStatWithAllStats ++ Seq(newComment):_*)
      trap.sections(0) must_== TrapSection(null,
          m("[thStat]*Detect* —\n" +
            "[thInit]*Initiative* -2\n" +
            "[thStat]*HP* 435\n" +
            "[thStat]*AC* 31, *Fortitude* —, *Reflex* 29, *Will* 30\n" +
            "[thStat]*Immune* attacks "))
      trap("stat:initiative") must_== Some("-2")
      trap("stat:hp") must_== Some("435")
      trap("stat:ac") must_== Some("31")
      trap("stat:fortitude") must_== Some("99")
      trap("stat:reflex") must_== Some("29")
      trap("stat:will") must_== Some("30")
    }

    "trap attack" in {
      val trap = readChunks(Seq(newHeader) ++ newStatNoInit ++ newTrapAttack ++ Seq(newComment):_*)
      trap.sections(0) must_== sectionHeadNoInit
      trap.sections(1) must_== sectionAttackNew
     }

    "trap with traits" in {
      val trap = readChunks(Seq(newHeader) ++ newStatNoInit ++ newTraits ++ Seq(newComment):_*)
      trap.sections(0) must_== sectionHeadNoInit
      trap.sections(1) must_== sectionNewTraits
     }
    "trap with countermeasure" in {
      val trap = readChunks(Seq(newHeader) ++ newStatNoInit ++ newTrapAttack ++ newCountermeasures ++ Seq(newComment):_*)
      trap.sections(0) must_== sectionHeadNoInit
      trap.sections(1) must_== sectionAttackNew
      trap.sections(2) must_== sectionCounterMeasures
     }
  }

  private def readChunks(nodes: Node*) = {
    val reader = new TrapReader(0)
    reader.process(parseChunks(nodes: _*))
  }

  private def extractSectionNames(sections: List[TrapSection]): List[String] = {
    sections.map(x => x.header)
  }

  private def parseChunks(xmlChunks: Node*):List[BlockElement] = xmlChunks.map(Parser.parseBlockElement).toList

  private def m(block: String) = MarkdownStyledTextParser.parse(block).right.get
}