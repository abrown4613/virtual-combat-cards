/**
 *  Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
import vcc.domain.dndi.Parser._
import vcc.infra.text.{TextSegment, TextBlock, StyledText}
import xml.Node
import MonsterBlockStreamRewrite.EndOfPower
import collection.mutable.ListBuffer


@RunWith(classOf[JUnitSuiteRunner])
class MonsterReaderTest extends JUnit4(MonsterReaderSpec)

object MonsterReaderSpec extends Specification {
  val reader = new MonsterReader(0)

  final val sampleDesc = StyledText(List(
    TextBlock("P","flavor",TextSegment("Something happens.")),
    TextBlock("P","flavorIndent",TextSegment.makeItalic("Secondary"))))

  // Returns a block stream with already advanced.
  def getBlockStream(xml: Node): TokenStream[BlockElement] = {
    val blk = Parser.parseBlockElement(xml, true)
    val blk2 = Block("P#flavor", List(Text("Something happens.")))
    val blk3 = Block("P#flavorIndent", List(Emphasis("Secondary")))
    val tailBlock = Block("POWEREND", Nil)

    val ts = new TokenStream(List(blk, blk2, blk3, tailBlock))
    ts.advance must beTrue
    ts
  }


  "MonsterReader.processTailBlock" should {
    "read description" in {
      val stream = new TokenStream[BlockElement](List(
        Block("P#flavor",List(Key("Description"),Text(": black and mean."), Break(), Text("Razor sharp")))
        ))
      stream.advance()
      val map = reader.processTailBlock(stream)

      map must haveKey("description")
      map("description") must_== "black and mean.\nRazor sharp"
    }

    "read trailing comment" in {
      // <p>Published in <a href="http://www.wizards.com/dnd/Product.aspx?x=dnd/products/dndacc/253840000" target="_new">Monster Manual 3</a>, page(s) 72.</p>
      val stream = new TokenStream[BlockElement](List(Block("P#",List(Text("Published in Monster Manual 3 , page(s) 72.")))))
      stream.advance()
      val map = reader.processTailBlock(stream)
      map must haveKey("comment")
      map("comment") must_== "Published in Monster Manual 3 , page(s) 72."
    }

    "read trailing comment in italic" in {
      // <p>Published in <a href="http://www.wizards.com/dnd/Product.aspx?x=dnd/products/dndacc/253840000" target="_new">Monster Manual 3</a>, page(s) 72.</p>
      val stream = new TokenStream[BlockElement](List(Block("P#",List(Emphasis("Published in Dungeon Magazine 155.")))))
      stream.advance()
      val map = reader.processTailBlock(stream)
      map must haveKey("comment")
      map("comment") must_== "Published in Dungeon Magazine 155."
    }
    "standalone MM<3 Equipment line" in {
      //<P class="flavor alt"><B>Equipment</B>: <A href="bla" target="_new">chainmail</A> , <A href="bla" target="_new">crossbow bolt</A>  x10, <A href="bla" target="_new">hand crossbow</A> , <A href="bla" target="_new">light shield</A> , <A href="bla" target="_new">spear</A> .</P>
      val stream = new TokenStream[BlockElement](List(Block("P#flavor alt",List(Key("Equipment"), Text(": chainmail , crossbow bolt  x10, hand crossbow , light shield , spear .")))))
      stream.advance()
      val map = reader.processTailBlock(stream)
      map must haveKey("equipment")
      map("equipment") must_== "chainmail , crossbow bolt  x10, hand crossbow , light shield , spear ."
    }

    "read equipment and alignment in MM3 format" in {
      //<P class="flavor"><B>Alignment</B> chaotic evil      <B> Languages</B> Common<BR/><B>Equipment</B>: <A href="bla" target="_new">club</A> , <A href="bla" target="_new">javelin</A>  x3, <A href="bla" target="_new">light shield</A> .</P>
      val stream = new TokenStream[BlockElement](List(Block("P#flavor",List(Key("Alignment"), Text(" chaotic evil      "), Key(" Languages"), Text(" Common"), Break(), Key("Equipment"), Text(": club , javelin  x3, light shield .")))))
      stream.advance()
      val map = reader.processTailBlock(stream)
      val expects = Map(
        "alignment" -> "chaotic evil",
        "languages" -> "Common",
        "equipment" -> "club , javelin  x3, light shield ."
        )
      for((k,v)<- expects) {
        map must haveKey(k)
        map(k) must_== v
      }
    }
    "STR block MM3" in {
      //<P class="flavor alt"><B>Str</B> 19 (+10)    <B>Dex</B> 19 (+10)     <B>Wis</B> 15 (+8)<BR/><B>Con</B> 20 (+11)        <B>Int</B> 8 (+5)     <B>Cha</B> 17 (+9)</P>
      val stream = new TokenStream[BlockElement](List(Block("P#flavor alt", List(Key("Skills"), Text(" Bluff +8, Stealth +9, Thievery +9"), Break(),
        Text(" "), Key("Str"), Text(" 19 (+10)    "), Key("Dex"), Text(" 19 (+10)     "), Key("Wis"), Text(" 15 (+8)"), Break(),
        Key("Con"), Text(" 20 (+11)        "), Key("Int"), Text(" 8 (+5)    "), Key("Cha"), Text(" 17 (+9)")))))
      stream.advance()
      val map = reader.processTailBlock(stream)
      val expects = Map(
        "skills" -> "Bluff +8, Stealth +9, Thievery +9",
        "str" -> "19 (+10)",
        "con" -> "20 (+11)",
        "cha" -> "17 (+9)")
      for((k,v)<- expects) {
        map must haveKey(k)
        map(k) must_== v
      }
    }
  }

  "MonsterReader.processHeader" should {
    "simplify minion role" in {
      val ts = new TokenStream[BlockElement](List(HeaderBlock("H1#monster",List(("name","Pest"), ("type","dude"), ("level","Level 1 Minion Controller"), ("xp","XP 25")))))
      ts.advance()
      val fields = reader.processHeader(ts)

      fields must_== Map(
        "name" -> "Pest", "type" -> "dude", "level" -> "1",
        "xp" -> "25", "role" -> "Controller")
    }
    "provide No Role for old minion format" in {
      val ts = new TokenStream[BlockElement](List(HeaderBlock("H1#monster",List(("name","Pest"), ("type","dude"), ("level","Level 1 Minion"), ("xp","XP 25")))))
      ts.advance()
      val fields = reader.processHeader(ts)
      
      fields must_== Map(
        "name" -> "Pest", "type" -> "dude", "level" -> "1",
        "xp" -> "25", "role" -> "No Role")
    }
    "leave other roles unchanged" in {
      val ts = new TokenStream[BlockElement](List(HeaderBlock("H1#monster",List(("name","Pest"), ("type","dude"), ("level","Level 1 Elite Brute (Leader)"), ("xp","XP 25")))))
      ts.advance()
      val fields = reader.processHeader(ts)

      fields must_== Map(
        "name" -> "Pest", "type" -> "dude", "level" -> "1",
        "xp" -> "25", "role" -> "Elite Brute (Leader)")
    }

  }

  "MonsterReader.processPrimaryBlock" should {
    "handle MM<3 entries and split auras out" in {
      val stb = Block("P#flavor", List(Key("Initiative"), Text("7"), Key("Senses"), Text("Perception +12; low-light vision"), Break(),
        Key("Aura name 1"), Text("(Fire) aura 1; Some long description about the aura."), Break(),
        Key("Aura name 2"), Text("(Thunder) aura 2; Some long description about the aura."), Break(),
        Key("HP"), Text("360"), Key("Bloodied"), Text("180"), Break(), Key("AC"), Text("23; "), Key("Fortitude"), Text("24, "), Key("Reflex"), Text("19,"), Key("Will"), Text("19"), Break(),
        Key("Resist"), Text("10 variable (1/encounter)"), Break(),
        Key("Saving Throws"), Text("+4"), Break(),
        Key("Speed"), Text("  8, climb 8  "), Break(),
        Key("Action Points"), Text("2")))

      val stream = new TokenStream[BlockElement](List(stb,EndOfPower)) //EndOfPower is uset to simplify testing, not in real life
      stream.advance()
      val (isMM3, map, auras) = reader.processMainStatBlock(stream)
      //This is a dummy call to make user processMainStatBlock advances
      stream.head must_== EndOfPower
      stream.advance() must beFalse // Be sure it move ahead
      isMM3 must beFalse
      val expects = Map(
        "senses" -> "low-light vision",
        "perception" -> "12",
        "hp" -> "360",
        "ac" -> "23",
        "reflex" -> "19",
        "saving throws" -> "4",
        "speed" -> "8, climb 8",
        "resist" -> "10 variable (1/encounter)")
      for ((k, v) <- expects) {
        map must haveKey(k)
        map(k) must_== v
      }

      // Check aura capture
      auras.length must_== 2
      auras(0)._1 must_== "Aura name 1"
      auras(0)._2 must_== "(Fire) aura 1; Some long description about the aura."
      auras(1)._1 must_== "Aura name 2"
    }


    "process tabular block" in {
      val blk = Table("bodytable", List(
        Cell(null, List(Key("HP"), Text(" 220; "), Key("Bloodied"), Text(" 110"))),
        Cell("rightalign", List(Key("Initiative"), Text(" +7"))),
        Cell(null, List(Key("AC"), Text(" 24, "), Key("Fortitude"), Text(" 22, "), Key("Reflex"), Text(" 20, "), Key("Will"), Text(" 22"))),
        Cell("rightalign", List(Key("Perception"), Text(" +15"))),
        Cell(null, List(Key("Speed"), Text(" 6"))),
        Cell("rightalign", List(Text("Blindsight 5"))),
        Cell(null, List(Key("Resist"), Text(" 5 necrotic"))),
        Cell(null, List(Key("Saving Throws"), Text(" +2; "), Key("Action Points"), Text(" 1")))))

      val stream = new TokenStream[BlockElement](List(blk, EndOfPower))
      stream.advance()
      val (isMM3, map, auras) = reader.processMainStatBlock(stream)

      //This is a dummy call to make user processMainStatBlock advances
      stream.head must_== EndOfPower
      stream.advance() must beFalse // Be sure it move ahead

      isMM3 must beTrue
      auras must beEmpty

      val expects = Map(
        "senses" -> "Blindsight 5",
        "hp" -> "220",
        "ac" -> "24",
        "reflex" -> "20",
        "saving throws" -> "2",
        "speed" -> "6",
        "perception" -> "15",
        "resist" -> "5 necrotic")
      for ((k, v) <- expects) {
        map must haveKey(k)
        map(k) must_== v
      }

    }

    "process tabular block with no Senses, saving, or resist" in {
      val blk = Table("bodytable", List(
        Cell(null, List(Key("HP"), Text(" 220; "), Key("Bloodied"), Text(" 110"))),
        Cell("rightalign", List(Key("Initiative"), Text(" +7"))),
        Cell(null, List(Key("AC"), Text(" 24, "), Key("Fortitude"), Text(" 22, "), Key("Reflex"), Text(" 20, "), Key("Will"), Text(" 22"))),
        Cell("rightalign", List(Key("Perception"), Text(" +15"))),
        Cell(null, List(Key("Speed"), Text(" 6"))),
        Cell("rightalign", List()),
        Cell(null, List())))

      val stream = new TokenStream[BlockElement](List(blk))
      stream.advance()
      val (isMM3, map, auras) = reader.processMainStatBlock(stream)
      isMM3 must beTrue
      auras must beEmpty

      val expects = Map(
        "hp" -> "220",
        "ac" -> "24",
        "reflex" -> "20",
        "speed" -> "6")
      for ((k, v) <- expects) {
        map must haveKey(k)
        map(k) must_== v
      }
      map must notHaveKey("senses")
      map must notHaveKey("saving throws")
      map must notHaveKey("resist")
    }
  }


  "MonsterReader.processPower" should {

    val mr = new MonsterReader(0)

    "read aura with keyword" in {
      val ts = getBlockStream(<P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/aura.png" align="top"></IMG> <B>Spider Host</B> (Poison) <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG> <B>Aura</B> 1</P>)
      val power = mr.processPower(ActionType.Trait, ts)
      power must notBeNull

      power.definition must_== CompletePowerDefinition(Seq(IconType.Aura), "Spider Host", "(Poison)", AuraUsage(1))
      power.action must_== ActionType.Trait
      power.description must_== sampleDesc
    }

    "read power without keyword" in {
      val ts = getBlockStream(<P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/Z3a.gif"></IMG> <B>Darkfire</B> <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG> <B>Encounter</B></P>)
      val power = mr.processPower(ActionType.Minor, ts)
      power must notBeNull

      power.definition must_== CompletePowerDefinition(Seq(IconType.Range), "Darkfire", null, EncounterUsage(1))
      power.action must_== ActionType.Minor
      power.description must_== sampleDesc
    }

    "failed broken power" in {
      val ts = getBlockStream(<P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/Z3a.gif"></IMG> <B>Bad</B> <B>Power</B> <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG> <B>Encounter</B></P>)
      mr.processPower(ActionType.Minor, ts) must throwAn[UnexpectedBlockElementException]
    }

  }

  "MonsterReader.processPowerGroup" should {
    "read a sequence of powers ending with EndOfPower" in {
      val ts = new TokenStream[BlockElement](generateMeleePower("hack") ::: generateMeleePower("slash") ::: List(EndOfPower))
      ts.advance()
      val powers = reader.processPowerGroup(null, ts)
      ts.head must_== EndOfPower
      powers.length must_== 2
      powers(0).definition.name must_== "hack"
      powers(1).definition.name must_== "slash"
    }
    "read a sequence of powers ending with a H2 header" in {
      val standardAction = Block("H2", List(Text("Standard Action")))
      val ts = new TokenStream[BlockElement](generateMeleePower("hack") ::: generateMeleePower("slash") ::: List(standardAction))
      ts.advance()
      val powers = reader.processPowerGroup(null, ts)
      ts.head must_== standardAction
      powers.length must_== 2
      powers(0).definition.name must_== "hack"
      powers(1).definition.name must_== "slash"
    }
  }

  "MonsterReader.processActionGroups" should {
    "read sequence of H2 separated powers" in {
      val traitAction = Block("H2#", List(Text("Traits")))
      val standardAction = Block("H2#", List(Text("Standard Actions")))
      val ts = new TokenStream[BlockElement](
        traitAction :: generateMeleePower("hack") ::: standardAction :: generateMeleePower("slash") ::: List(EndOfPower))
      ts.advance()
      val powersByAction = reader.processActionGroups(ts)
      ts.head must_== EndOfPower
      powersByAction.length must_== 2
      powersByAction(0)._1 must_== ActionType.Trait
      powersByAction(0)._2(0).definition.name must_== "hack"
      powersByAction(0)._2(0).action must_== ActionType.Trait
      powersByAction(1)._1 must_== ActionType.Standard
      powersByAction(1)._2(0).action must_== ActionType.Standard
      powersByAction(1)._2(0).definition.name must_== "slash"
    }
  }

  "MonsterBlockStream" should {

    "insert ENDPOWER block before block with starting with Skill" in {
      val blk = Block("P#flavor alt", List(Key("Skills"), Text("Arcana +13, Bluff +12"), Break(), Key("Str"), Text("12 (+5)"), Key("Dex"), Text("14 (+6)"), Key("Wis"), Text("19 (+8)"), Break(), Key("Con"), Text("17 (+7)"), Key("Int"), Text("19 (+8)"), Key("Cha"), Text("16 (+7)")))

      val ts = new TokenStream[BlockElement](List(blk), new MonsterBlockStreamRewrite())
      ts.advance() must beTrue
      ts.head must_== Block("ENDPOWER",Nil)
      ts.advance() must beTrue
      ts.head must_== blk
    }

    "insert ENDPOWER block before block with Str" in {
      val blk = Block("P#flavor alt", List(Key("Str"), Text("12 (+5)"), Key("Dex"), Text("14 (+6)"), Key("Wis"), Text("19 (+8)"), Break(), Key("Con"), Text("17 (+7)"), Key("Int"), Text("19 (+8)"), Key("Cha"), Text("16 (+7)")))
      val ts = new TokenStream[BlockElement](List(blk), new MonsterBlockStreamRewrite())
      ts.advance() must beTrue
      ts.head must_== Block("ENDPOWER",Nil)
      ts.advance() must beTrue
      ts.head must_== blk
    }

    "insert ENDPOWER block before block with Aligment" in {
      val blk = Block("P#flavor alt", List(Key("Alignment"), Text("unaligned"), Key("Languages"), Text("-"), Break(), Key("Str"), Text("12 (+5)"), Key("Dex"), Text("14 (+6)")))
      val ts = new TokenStream[BlockElement](List(blk), new MonsterBlockStreamRewrite())
      ts.advance() must beTrue
      ts.head must_== Block("ENDPOWER", Nil)
      ts.advance() must beTrue
      ts.head must_== blk
    }

    "but only add one ENDPOWER" in {
      val blk = Block("P#flavor alt", List(Key("Str"), Text("12 (+5)"), Key("Dex"), Text("14 (+6)"), Key("Wis"), Text("19 (+8)"), Break(), Key("Con"), Text("17 (+7)"), Key("Int"), Text("19 (+8)"), Key("Cha"), Text("16 (+7)")))
      val ts = new TokenStream[BlockElement](List(blk, blk), new MonsterBlockStreamRewrite())
      ts.advance() must beTrue
      ts.head must_== Block("ENDPOWER", Nil)
      ts.advance() must beTrue
      ts.head must_== blk
      ts.advance() must beTrue
      ts.head must_== blk
    }
    "return normal 'flavor alt' when not power end" in {
      val phl = (<P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/S2.gif"></IMG><B>Mace</B> (standard, at-will) <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG><B>Arcane, Weapon</B></P>)
      val blk = Parser.parseBlockElement(phl, true)
      val ts = new TokenStream[BlockElement](List(blk), new MonsterBlockStreamRewrite())
      ts.advance() must beTrue
      ts.head must_== blk
    }

    "consume any NonBlock" in {
      val blk = Block("P#",List(Text("Hello.")))
      val ts = new TokenStream[BlockElement](List(NonBlock(List(Break())),blk), new MonsterBlockStreamRewrite())
      ts.advance() must beTrue
      ts.head must_== blk
    }
  }

  "MonsterReader.promoteAuraLike" should {
    val auraDesc = StyledText.singleBlock("P", "flavorIndent", "some description.")
    "lift simple aura to new power" in {
      val power = reader.promoteAuraLike("Mocking Eye", "aura 10; some description.")
      power mustNot beNull
      power.action must_== ActionType.Trait
      power.definition must_== CompletePowerDefinition(Seq(IconType.Aura), "Mocking Eye", null, AuraUsage(10))
      power.description must_== auraDesc
    }
    "lift simple aura with keyword to new power" in {
      val power = reader.promoteAuraLike("Aura of Terror", "(Fear) aura 5; some description.")
      power mustNot beNull
      power.action must_== ActionType.Trait
      power.definition must_== CompletePowerDefinition(Seq(IconType.Aura), "Aura of Terror", "(Fear)", AuraUsage(5))
      power.description must_== auraDesc
    }

    "lift regeneration with only a number" in {
      val power = reader.promoteAuraLike("Regeneration", "10")
      power mustNot beNull
      power.action must_== ActionType.Trait
      power.definition must_== CompletePowerDefinition(Seq(), "Regeneration", null, NoUsage)
      power.description must_== StyledText(List(TextBlock("P", "flavorIndent", TextSegment("10"))))
    }
    "lift regeneration with a description" in {
      val power = reader.promoteAuraLike("Regeneration", "3 (if the werewolf takes damage from a silver weapon)")
      power mustNot beNull
      power.action must_== ActionType.Trait
      power.definition must_== CompletePowerDefinition(Seq(), "Regeneration", null, NoUsage)
      power.description must_== StyledText(List(TextBlock("P", "flavorIndent", TextSegment("3 (if the werewolf takes damage from a silver weapon)"))))
    }
  }

  "MonsterReader.normalizeLegacySenses" should {

    "leave other unchanged" in {
      val senses = Map("hp" -> "100", "ac" -> "25")
      val norm = reader.normalizeLegacySenses(senses)
      norm must_== senses
    }
    "normalize broken Senses" in {
      val norm = reader.normalizeLegacySenses(Map("hp" -> "100", "senses" -> "; low-light vision"))
      norm must_== Map("hp" -> "100", "perception" -> "0", "senses" -> "low-light vision")
    }
    "normalize Senses with two modes" in {
      val norm = reader.normalizeLegacySenses(Map("hp" -> "100", "senses" -> "Perception +10; blindsight 10, tremorsense 20"))
      norm must_== Map("hp" -> "100", "perception" -> "10", "senses" -> "blindsight 10, tremorsense 20")
    }
    "normalize Senses with one modes" in {
      val norm = reader.normalizeLegacySenses(Map("hp" -> "100", "senses" -> "Perception +10; darkvision"))
      norm must_== Map("hp" -> "100", "perception" -> "10", "senses" -> "darkvision")
    }
    "normalize Senses with one modes, negative perception" in {
      val norm = reader.normalizeLegacySenses(Map("hp" -> "100", "senses" -> "Perception -10; darkvision"))
      norm must_== Map("hp" -> "100", "perception" -> "-10", "senses" -> "darkvision")
    }
    "normalize Senses with no modes" in {
      val norm = reader.normalizeLegacySenses(Map("hp" -> "100", "senses" -> "Perception +10"))
      norm must_== Map("hp" -> "100", "perception" -> "10")
      norm mustNot haveKey("senses")
    }
  }
  /*
  * This is a conditional test that will iterate through all cached entris in a directory.
  */
  if (System.getProperty("test.basedir") != null) {
    val dir = new java.io.File(System.getProperty("test.basedir"))
    val failures = new ListBuffer[String]
    "DNDI Importer" should {
      "load everybody in " + dir.getAbsolutePath in {
        val dirIter = new vcc.util.DirectoryIterator(dir, false)
        for (file <- dirIter if (file.isFile)) {
          val testedOk: Boolean = try {
            val xml = scala.xml.XML.loadFile(file)
            val blocks = parseBlockElements(xml.child, true)
            if (blocks != null && !blocks.isEmpty) {
              val mReader = new MonsterReader(0)
              val monster = mReader.process(blocks)
              monster != null
            } else false
          } catch {
            case e =>
              System.err.println("Failed to parse: " + file + " reason: " + e.getMessage)
              //e.printStackTrace
              false
          }
          if (!testedOk.isExpectation) {
            failures += ("loading failed for " + file)
          }
        }
        if (!failures.isEmpty) fail("Failed to process:\n" + failures.mkString("\n"))
      }
    }
  }

  def generateMeleePower(name:String):List[BlockElement] = {
    val header = Block("P#flavor alt", List(Icon(IconType.Melee), Text(" "), Key(name), Text(" "), Icon(IconType.Separator), Text(" "), Key("Encounter")))
    val blk = Block("P#flavor", List(Text("Something happens.")))
    List(header, blk)
  }

}