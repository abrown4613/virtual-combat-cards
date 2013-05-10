/*
 *  Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
import vcc.dndi.reader.Parser._
import vcc.infra.text.{TextSegment, TextBlock, StyledText}
import xml.Node
import collection.mutable.ListBuffer
import java.io.File
import org.specs2.matcher.Matcher

class MonsterReaderTest extends SpecificationWithJUnit {
  private final val sampleDesc = StyledText(List(
    TextBlock("P", "flavor", TextSegment("Something happens.")),
    TextBlock("P", "flavorIndent", TextSegment.makeItalic("Secondary"))))

  "MonsterReader.processTailBlock" should {
    "read monster with description with breaks" in {
      def descriptionLine = MonsterChunk(Seq(Block( "P#flavor", List(Key("Description"), Text(": black and mean."), Break(), Text("Razor sharp")))),
        haveAttribute("text:description", "black and mean.\nRazor sharp"))
      parseMonster(pestMinionHead, mm3PrimaryStatSimple, secondaryStats, descriptionLine, alignmentLine)
    }

    "read monster with styled comment" in {
      val comment = "Published in Monster Manual 3 , page(s) 72."
      parseMonster(pestMinionHead, mm3PrimaryStatSimple, secondaryStats, alignmentLine,
        makeCommentVariant(Block("P#publishedIn", List(Text(comment))), comment))
    }

    "read monster with comment" in {
      val comment = "Some book somewhere"
      parseMonster(pestMinionHead, mm3PrimaryStatSimple, secondaryStats, alignmentLine,
        makeCommentVariant(Block("P#", List(Text(comment))), comment))
    }

    "read monster with comment in italic" in {
      val comment = "Some book somewhere else"
      parseMonster(pestMinionHead, mm3PrimaryStatSimple, secondaryStats, alignmentLine,
        makeCommentVariant(Block("P#", List(Emphasis(comment))), comment))
    }

    "standalone MM2 Equipment line" in {
      //<P class="flavor alt"><B>Equipment</B>: <A href="bla" target="_new">chainmail</A> , <A href="bla" target="_new">crossbow bolt</A>  x10, <A href="bla" target="_new">hand crossbow</A> , <A href="bla" target="_new">light shield</A> , <A href="bla" target="_new">spear</A> .</P>
      val equipLine = MonsterChunk(Seq(Block("P#flavor alt", List(Key("Equipment"), Text(": chainmail , crossbow bolt  x10, hand crossbow , light shield , spear .")))),
        haveAttribute("stat:equipment", "chainmail , crossbow bolt  x10, hand crossbow , light shield , spear ."))
      parseMonster(pestMinionHead, mm2StatBlock1WithoutAura, secondaryStats,equipLine)
    }

    "read equipment and alignment in MM3 format" in {
      //<P class="flavor"><B>Alignment</B> chaotic evil      <B> Languages</B> Common<BR/><B>Equipment</B>: <A href="bla" target="_new">club</A> , <A href="bla" target="_new">javelin</A>  x3, <A href="bla" target="_new">light shield</A> .</P>
      val stream = MonsterChunk(Seq(Block("P#flavor", List(Key("Alignment"), Text(" chaotic evil      "), Key(" Languages"), Text(" Common"), Break(), Key("Equipment"), Text(": club , javelin  x3, light shield .")))),
        mustHaveAllAttributes(Map(
          "stat:alignment" -> "chaotic evil",
          "stat:languages" -> "Common",
          "stat:equipment" -> "club , javelin  x3, light shield .")))
      parseMonster(pestMinionHead, mm2StatBlock1WithoutAura, secondaryStats,stream)
    }
  }

  "MonsterReader.processHeader" should {
    "simplify minion role" in {
      val head = MonsterChunk(Seq(
        HeaderBlock("H1#monster", List(("name", "Pest"), ("type", "dude"), ("level", "Level 1 Minion Controller"), ("xp", "XP 25")))),
        mustHaveAllAttributes(Map(
          "base:name" -> "Pest", "base:type" -> "dude", "base:level" -> "1",
          "base:xp" -> "25", "base:role" -> "Controller")))
      parseMonster(head, mm3PrimaryStatSimple, secondaryStats, alignmentLine)
    }

    "provide No Role for old minion format" in {
      val head = MonsterChunk(Seq(
        HeaderBlock("H1#monster", List(("name", "Pest"), ("type", "dude"), ("level", "Level 12 Minion"), ("xp", "XP 1800")))),
        mustHaveAllAttributes(Map(
          "base:name" -> "Pest", "base:type" -> "dude", "base:level" -> "12",
          "base:xp" -> "1800", "base:role" -> "No Role")))
      parseMonster(head, mm3PrimaryStatSimple, secondaryStats, alignmentLine)
    }

    "parse header block with no level or XP" in {
      //<H1 class="monster">Pest<BR></BR><SPAN class="type">animate </SPAN><BR></BR> </H1>
      val head = MonsterChunk(Seq(
        HeaderBlock("H1#monster", List(("name", "Pest"),("type", "animate")))),
        mustHaveAllAttributes(Map(
          "base:name" -> "Pest", "base:type" -> "animate", "base:level" -> "1",
          "base:xp" -> "0", "base:role" -> "No Role")))
      parseMonster(head, mm3PrimaryStatSimple, secondaryStats, alignmentLine)
    }

    "leave other roles unchanged" in {
      val head = MonsterChunk(Seq(
        HeaderBlock("H1#monster", List(("name", "Bob"), ("type", "Orc"), ("level", "Level 3 Elite Brute (Leader)"), ("xp", "XP 250")))),
        mustHaveAllAttributes(Map(
          "base:name" -> "Bob", "base:type" -> "Orc", "base:level" -> "3",
          "base:xp" -> "250", "base:role" -> "Elite Brute (Leader)")))
      parseMonster(head, mm3PrimaryStatSimple, secondaryStats, alignmentLine)
    }

    "change xp of - to 0" in {
      val head = MonsterChunk(Seq(
        HeaderBlock("H1#monster", List(("name", "Pest"), ("type", "dude"), ("level", "Level 1 Soldier"), ("xp", "-")))),
        mustHaveAllAttributes(Map(
          "base:name" -> "Pest", "base:type" -> "dude", "base:level" -> "1",
          "base:xp" -> "0", "base:role" -> "Soldier")))
      parseMonster(head, mm3PrimaryStatSimple, secondaryStats, alignmentLine)
    }
  }

  private def mm2StatBlock1WithoutAura = MonsterChunk(
    Seq(
      Block("P#flavor", List(Key("Initiative"), Text("7"), Key("Senses"), Text("Perception +12; low-light vision"), Break(),
        Key("HP"), Text("360"), Key("Bloodied"), Text("180"), Break(),
        Key("AC"), Text("23; "), Key("Fortitude"), Text("24, "), Key("Reflex"), Text("19,"), Key("Will"), Text("18"), Break(),
        Key("Resist"), Text("10 variable (1/encounter)"), Break(),
        Key("Saving Throws"), Text("+4"), Break(),
        Key("Speed"), Text("  8, climb 8  "), Break(),
        Key("Action Points"), Text("2")))
    ),
    mustHaveAllAttributes(Map(
      "stat:senses" -> "low-light vision", "stat:perception" -> "12",
      "stat:hp" -> "360", "stat:initiative" -> "7",
      "stat:ac" -> "23", "stat:fortitude" -> "24", "stat:reflex" -> "19", "stat:will" -> "18",
      "stat:saving throws" -> "4", "stat:action points" -> "2", "stat:speed" -> "8, climb 8",
      "stat:resist" -> "10 variable (1/encounter)")))

  private def havePower(action: ActionType.Value, order: Int, powerMatcher: Matcher[Power]):Matcher[Monster] =
    (beDefinedAt(action) ^^ {monster: Monster => monster.powersByAction}).
      setMessage("Monster does not have powers of type " + action) and
    (be_>(order) ^^ {monster: Monster => monster.powersByAction(action).length}).
      setMessage("Monster does not have " + order + " powers of type " + action) and
    (powerMatcher ^^ { monster: Monster => monster.powersByAction(action)(order)})

  private def haveLegacyPower(order: Int, powerMatcher: Matcher[Power]):Matcher[Monster] =
    (be_>(order) ^^ {monster: Monster => monster.legacyPowers.length}).
      setMessage("Monster does not have " + order + " legacy powers") and
    (powerMatcher ^^ { monster: Monster => monster.legacyPowers(order)})

  private def auraLike(name: String, keyword: String, range: String, description: String): Matcher[Power] =
    (havePowerDefinition(CompletePowerDefinition(Seq(IconType.Aura), name, if (keyword != null) keyword.trim else null, AuraUsage(range))) ^^ {
      power: Power => power.definition
    }) and
      (be_==(StyledText.singleBlock("P", "flavorIndent", description)) ^^ {
        power: Power => power.description
      }).
        updateMessage(_ + " in Power Description")

  private def likeRegeneration(description: String): Matcher[Power] =
    (havePowerDefinition(CompletePowerDefinition(Seq(), "Regeneration", null, NoUsage)) ^^
      { power: Power => power.definition}) and
      (be_==(StyledText.singleBlock("P", "flavorIndent", description)) ^^
        { power: Power => power.description}).
        updateMessage(_ + " in Power Description")

  private def havePowerDefinition(definition:PowerDefinition):Matcher[PowerDefinition] =
    (be_==(definition.name) ^^ {dp: PowerDefinition => dp.name}) and
    (be_==(definition.icons) ^^ {dp: PowerDefinition => dp.icons}) and
    (be_==(definition.keyword) ^^ {dp: PowerDefinition => dp.keyword}) and
    (be_==(definition.usage) ^^ {dp: PowerDefinition => dp.usage})

  "MonsterReader.processPrimaryBlock" should {

    "handle monster in format prior to MM3" in {
      parseMonster(pestMinionHead, mm2StatBlock1WithoutAura, secondaryStats, alignmentLine)
    }

    "handle MM<3 entries and split auras out" in {
      val mm2WithAuras = MonsterChunk(Seq(
        Block("P#flavor", List(Key("Initiative"), Text("7"), Key("Senses"), Text("Perception +12; low-light vision"), Break(),
          Key("Aura name 1"), Text("(Fire) aura 1; Some long description about the aura."), Break(),
          Key("Aura name 2"), Text("aura 2; Some long description about the aura."), Break(),
          Key("HP"), Text("460"), Key("Bloodied"), Text("230"), Break(),
          Key("AC"), Text("25; "), Key("Fortitude"), Text("24, "), Key("Reflex"), Text("23,"), Key("Will"), Text("22"), Break(),
          Key("Resist"), Text("10 variable (1/encounter)"), Break(),
          Key("Saving Throws"), Text("+4"), Break(),
          Key("Speed"), Text("  8, climb 8  "), Break()))),
        mustHaveAllAttributes(Map(
          "stat:senses" -> "low-light vision", "stat:perception" -> "12", "stat:hp" -> "460",
          "stat:ac" -> "25", "stat:fortitude" -> "24", "stat:reflex" -> "23", "stat:will" -> "22",
          "stat:saving throws" -> "4", "stat:speed" -> "8, climb 8", "stat:resist" -> "10 variable (1/encounter)")) and
          havePower(ActionType.Trait,0, auraLike("Aura name 1", "(Fire)", "1", "Some long description about the aura.")) and
          havePower(ActionType.Trait,1, auraLike("Aura name 2", null, "2", "Some long description about the aura.")))

      parseMonsterAndDump(pestMinionHead, mm2WithAuras, secondaryStats, alignmentLine)
    }

    "handle mm3 tabular block" in {
      val mm3Primary = MonsterChunk(Seq(
        Table("bodytable", List(
          Cell(null, List(Key("HP"), Text(" 220; "), Key("Bloodied"), Text(" 110"))),
          Cell("rightalign", List(Key("Initiative"), Text(" +7"))),
          Cell(null, List(Key("AC"), Text(" 24, "), Key("Fortitude"), Text(" 22, "), Key("Reflex"), Text(" 20, "), Key("Will"), Text(" 21"))),
          Cell("rightalign", List(Key("Perception"), Text(" +15"))),
          Cell(null, List(Key("Speed"), Text(" 6"))),
          Cell("rightalign", List(Text("Blindsight 5"))),
          Cell(null, List(Key("Resist"), Text(" 5 necrotic"))),
          Cell(null, List(Key("Saving Throws"), Text(" +2; "), Key("Action Points"), Text(" 1")))))),
        mustHaveAllAttributes(Map(
          "stat:hp" -> "220", "stat:initiative" -> "7",
          "stat:ac" -> "24", "stat:fortitude" -> "22", "stat:reflex" -> "20", "stat:will" -> "21",
          "stat:saving throws" -> "2", "stat:speed" -> "6", "stat:perception" -> "15", "stat:senses" -> "Blindsight 5",
          "stat:resist" -> "5 necrotic", "stat:saving throws" -> "2", "stat:action points" -> "1")))
      parseMonster(pestMinionHead, mm3Primary, secondaryStats, alignmentLine)
    }

    "process tabular block with no Senses, saving, or resist" in {
      val mm3NoSenseSaveResist =MonsterChunk(Seq(
        Table("bodytable", List(
          Cell(null, List(Key("HP"), Text(" 220; "), Key("Bloodied"), Text(" 110"))),
          Cell("rightalign", List(Key("Initiative"), Text(" +7"))),
          Cell(null, List(Key("AC"), Text(" 24, "), Key("Fortitude"), Text(" 23, "), Key("Reflex"), Text(" 22, "), Key("Will"), Text(" 21"))),
          Cell("rightalign", List(Key("Perception"), Text(" +15"))),
          Cell(null, List(Key("Speed"), Text(" 6"))),
          Cell("rightalign", List()),
          Cell(null, List())))),
        aggregateMatchers(
          mustHaveAllAttributes(Map(
            "stat:hp" -> "220", "stat:initiative" -> "7",
            "stat:ac" -> "24", "stat:fortitude" -> "23", "stat:reflex" -> "22", "stat:will" -> "21",
            "stat:speed" -> "6", "stat:perception" -> "15")),
          notHaveAttribute("stat:senses"),
          notHaveAttribute("stat:saving throws"),
          notHaveAttribute("stat:resist")
        ))
      parseMonster(pestMinionHead, mm3NoSenseSaveResist, secondaryStats, alignmentLine)
    }
  }

  private def getBlocks(xml: Node): List[BlockElement] = {
    val blk = Parser.parseBlockElement(xml)
    val blk2 = Block("P#flavor", List(Text("Something happens.")))
    val blk3 = Block("P#flavorIndent", List(Emphasis("Secondary")))
    List(blk, blk2, blk3)
  }

  def likePower(definition: PowerDefinition, action: ActionType.Value, description: StyledText):Matcher[Power] =
    (havePowerDefinition(definition) ^^ {power: Power => power.definition }) and
    (be_==(action) ^^ {power: Power => power.action}) and
    (be_==(description) ^^ {power: Power => power.description})

  "MonsterReader.processPower" should {

    "read mm3 aura with keyword" in {
      val ts = getBlocks(<P class="flavor alt"> <IMG src="http://www.wizards.com/dnd/images/symbol/aura.png" align="top"></IMG> <B>Spider Host</B> (Poison) <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG> <B>Aura</B> 1</P>)
      val auraPower = MonsterChunk(
        Block("H2#", List(Text("Traits"))) :: ts,
        havePower(ActionType.Trait, 0, likePower(
          CompletePowerDefinition(Seq(IconType.Aura), "Spider Host", "(Poison)", AuraUsage("1")),
          ActionType.Trait,
          sampleDesc)))
      parseMonster(pestMinionHead, mm3PrimaryStatSimple, auraPower, secondaryStats, alignmentLine)
    }

    "read power without keyword" in {
      val ts = getBlocks(<P class="flavor alt"> <IMG src="http://www.wizards.com/dnd/images/symbol/Z3a.gif"></IMG> <B>Darkfire</B> <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG> <B>Encounter</B> </P>)
      val minorPower = MonsterChunk(
        Block("H2#", List(Text("Minor Actions"))) :: ts,
        havePower(ActionType.Minor, 0, likePower(
          CompletePowerDefinition(Seq(IconType.Range), "Darkfire", null, EncounterUsage()),
          ActionType.Minor,
          sampleDesc)))
      parseMonster(pestMinionHead, mm3PrimaryStatSimple, minorPower, secondaryStats, alignmentLine)
    }

    "failed broken power" in {
      val ts = getBlocks(<P class="flavor alt"> <IMG src="http://www.wizards.com/dnd/images/symbol/Z3a.gif"></IMG> <B>Bad</B> <B>Power</B> <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG> <B>Encounter</B> </P>)
      val brokenPower = MonsterChunk(
        Block("H2#", List(Text("Minor Actions"))) :: ts,
        havePower(ActionType.Minor, 0, likePower(
          CompletePowerDefinition(Seq(IconType.Range), "Darkfire", null, EncounterUsage()),
          ActionType.Minor,
          sampleDesc)))
      val blocks = Seq(pestMinionHead, mm3PrimaryStatSimple, brokenPower, secondaryStats, alignmentLine).flatMap(_.blocks).toList
      new MonsterReader(0).process(blocks) must throwAn[UnexpectedBlockElementException]
    }
  }

  "MonsterReader.processPowerGroup" should {
    "Process legacy powers in a MM2 format" in {
      val parts = Seq(
        generateLegacyMeleePowerAndTest("Hack"),
        generateLegacyMeleePowerAndTest("Stab"),
        generateLegacyMeleePowerAndTest("Sling"))
      val blocks = parts.flatMap(_._1)
      val partMatcher = parts.map(_._2).zipWithIndex.map(m => haveLegacyPower(m._2, m._1))

      val allPowers = MonsterChunk(blocks, aggregateMatchers(partMatcher: _*))
      parseMonster(pestMinionHead, mm2StatBlock1WithoutAura, allPowers, secondaryStats, alignmentLine)
    }

    "mm3 read sequence of H2 separated powers" in {
      val traitsPower = makePowerGroup(ActionType.Trait, "Traits",
        generateMeleePowerAndTest("Hack", ActionType.Trait))
      val standardPowers = makePowerGroup(ActionType.Standard, "Standard Actions",
        generateMeleePowerAndTest("Stab", ActionType.Standard),
        generateMeleePowerAndTest("Sling", ActionType.Standard))
      parseMonsterAndDump(pestMinionHead, mm3PrimaryStatSimple, traitsPower, standardPowers, secondaryStats, alignmentLine)
    }
  }

  private def makePowerGroup(action: ActionType.Value, groupLabel: String, parts: (Seq[BlockElement], Matcher[Power])*) = {
    val header = Block("H2#", List(Text(groupLabel)))
    val blocks = parts.flatMap(_._1)
    val partMatcher = parts.map(_._2).zipWithIndex.map(m => havePower(action, m._2, m._1))
    MonsterChunk(Seq(header) ++ blocks, aggregateMatchers(partMatcher: _*))
  }

  "MonsterBlockStream" should {

    "insert ENDPOWER block before block with starting with Skill" in {
      val blk = Block("P#flavor alt", List(Key("Skills"), Text("Arcana +13, Bluff +12"), Break(), Key("Str"), Text("12 (+5)"), Key("Dex"), Text("14 (+6)"), Key("Wis"), Text("19 (+8)"), Break(), Key("Con"), Text("17 (+7)"), Key("Int"), Text("19 (+8)"), Key("Cha"), Text("16 (+7)")))

      val ts = new TokenStream[BlockElement](List(blk), new MonsterBlockStreamRewrite())
      ts.advance() must beTrue
      ts.head must_== Block("ENDPOWER", Nil)
      ts.advance() must beTrue
      ts.head must_== blk
    }

    "insert ENDPOWER block before block with Str" in {
      val blk = Block("P#flavor alt", List(Key("Str"), Text("12 (+5)"), Key("Dex"), Text("14 (+6)"), Key("Wis"), Text("19 (+8)"), Break(), Key("Con"), Text("17 (+7)"), Key("Int"), Text("19 (+8)"), Key("Cha"), Text("16 (+7)")))
      val ts = new TokenStream[BlockElement](List(blk), new MonsterBlockStreamRewrite())
      ts.advance() must beTrue
      ts.head must_== Block("ENDPOWER", Nil)
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
      val phl = (<P class="flavor alt"> <IMG src="http://www.wizards.com/dnd/images/symbol/S2.gif"></IMG> <B>Mace</B> (standard, at-will) <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG> <B>Arcane, Weapon</B> </P>)
      val blk = Parser.parseBlockElement(phl)
      val ts = new TokenStream[BlockElement](List(blk), new MonsterBlockStreamRewrite())
      ts.advance() must beTrue
      ts.head must_== blk
    }

    "consume any NonBlock" in {
      val blk = Block("P#", List(Text("Hello.")))
      val ts = new TokenStream[BlockElement](List(NonBlock(List(Break())), blk), new MonsterBlockStreamRewrite())
      ts.advance() must beTrue
      ts.head must_== blk
    }
  }

  "MonsterReader.promoteAuraLike" should {
    "lift regeneration" in {
      val mm2WithAuras = MonsterChunk(Seq(
        Block("P#flavor", List(Key("Initiative"), Text("7"), Key("Senses"), Text("Perception +12; low-light vision"), Break(),
          Key("HP"), Text("460"), Key("Bloodied"), Text("230"), Break(),
          Key("Regeneration"), Text("10"), Break(),
          Key("Regeneration"), Text("3 (if the werewolf takes damage from a silver weapon)"), Break(),
          Key("AC"), Text("25; "), Key("Fortitude"), Text("24, "), Key("Reflex"), Text("23,"), Key("Will"), Text("22"), Break(),
          Key("Resist"), Text("10 variable (1/encounter)"), Break(),
          Key("Saving Throws"), Text("+4"), Break(),
          Key("Speed"), Text("  8, climb 8  "), Break()))),
        mustHaveAllAttributes(Map(
          "stat:senses" -> "low-light vision", "stat:perception" -> "12", "stat:hp" -> "460",
          "stat:ac" -> "25", "stat:fortitude" -> "24", "stat:reflex" -> "23", "stat:will" -> "22",
          "stat:saving throws" -> "4", "stat:speed" -> "8, climb 8", "stat:resist" -> "10 variable (1/encounter)")) and
          havePower(ActionType.Trait, 0, likeRegeneration("10")) and
          havePower(ActionType.Trait, 1, likeRegeneration("3 (if the werewolf takes damage from a silver weapon)")))

      parseMonsterAndDump(pestMinionHead, mm2WithAuras, secondaryStats, alignmentLine)
    }
  }

  private case class MonsterChunk(blocks:Seq[BlockElement], matcher: Matcher[Monster])

  private def pestMinionHead = MonsterChunk(Seq(HeaderBlock("H1#monster", List(("name", "Pest"), ("type", "dude"), ("level", "Level 1 Minion Controller"), ("xp", "XP 25")))),
    mustHaveAllAttributes(Map(
      "base:name" -> "Pest", "base:type" -> "dude", "base:level" -> "1",
      "base:xp" -> "25", "base:role" -> "Controller"))
  )

  private def mm3PrimaryStatSimple = MonsterChunk(Seq(
    Table("bodytable", List(
      Cell(null, List(Key("HP"), Text(" 220; "), Key("Bloodied"), Text(" 110"))),
      Cell("rightalign", List(Key("Initiative"), Text(" +7"))),
      Cell(null, List(Key("AC"), Text(" 24, "), Key("Fortitude"), Text(" 23, "), Key("Reflex"), Text(" 22, "), Key("Will"), Text(" 21"))),
      Cell("rightalign", List(Key("Perception"), Text(" +15"))),
      Cell(null, List(Key("Speed"), Text(" 6"))),
      Cell("rightalign", List()),
      Cell(null, List())))),
    aggregateMatchers(mustHaveAllAttributes(Map(
      "stat:hp" -> "220", "stat:initiative" -> "7",
      "stat:ac" -> "24", "stat:fortitude" -> "23", "stat:reflex" -> "22", "stat:will" -> "21",
      "stat:perception" -> "15", "stat:speed" -> "6")),
      notHaveAttribute("stat:senses"),
      notHaveAttribute("stat:saving throws"),
      notHaveAttribute("stat:resist")))

  private def secondaryStats = MonsterChunk(Seq(
    Block("P#flavor alt", List(Key("Skills"), Text(" Bluff +8, Stealth +9, Thievery +19"), Break(),
      Text(" "), Key("Str"), Text(" 19 (+10)    "), Key("Dex"), Text(" 19 (+10)     "), Key("Wis"), Text(" 15 (+8)"), Break(),
      Key("Con"), Text(" 20 (+11)        "), Key("Int"), Text(" 8 (+5)    "), Key("Cha"), Text(" 17 (+9)")))),
    mustHaveAllAttributes(Map(
      "stat:skills" -> "Bluff +8, Stealth +9, Thievery +19",
      "stat:str" -> "19 (+10)", "stat:dex" -> "19 (+10)", "stat:con" -> "20 (+11)",
      "stat:cha" -> "17 (+9)", "stat:int" -> "8 (+5)", "stat:wis" -> "15 (+8)")))

  private def alignmentLine = MonsterChunk(
    Seq(Block("P#flavor", List(Key("Alignment"), Text(" chaotic evil      "), Key(" Languages"), Text(" Common"), Break(),
      Key("Equipment"), Text(": club , javelin  x3, light shield .")))),
    mustHaveAllAttributes(Map(
      "stat:equipment" -> "club , javelin  x3, light shield .",
      "stat:alignment" -> "chaotic evil",
      "stat:languages" -> "Common")))

  private def makeCommentVariant(block: BlockElement, comment:String) =
    MonsterChunk(Seq(block), haveAttribute("text:comment", comment))

  private def parseMonster(parts: MonsterChunk*) = {
    val (blocks, matcher) = (parts.flatMap(_.blocks).toList, aggregateMatchers(parts.map(_.matcher).toSeq: _*))
    val monster = new MonsterReader(0).process(blocks)
    monster must matcher
  }

  private def parseMonsterAndDump(parts: MonsterChunk*) = {
    val reader = new MonsterReader(0)
    val (blocks, matcher) = (parts.flatMap(_.blocks).toList, aggregateMatchers(parts.map(_.matcher).toSeq: _*))
    val monster = reader.process(blocks)
    monster.dump(System.out)
    println(monster.legacyPowers)
    println(monster.powersByAction)
    monster must matcher
  }

  private def mustHaveAllAttributes(expected: Map[String,String]): Matcher[Monster] =
    aggregateMatchers(expected.map(p => haveAttribute(p._1, p._2)).toSeq: _*)

  private def aggregateMatchers[T](matchers: Matcher[T]*):Matcher[T] = matchers.tail.foldLeft(matchers.head)(_ and _)

  private def haveAttribute(stat: String, value:String) : Matcher[Monster] =
    (beSome ^^ { t: Monster => t(stat) }).setMessage("Attribute "  + stat + " not defined") and
      (be_==(value) ^^ { t: Monster => t(stat).get}).updateMessage("Attribute " + stat + " mismatch: " +_)

  private def notHaveAttribute(stat: String) : Matcher[Monster] =
    (beNone ^^ {t: Monster => t(stat)}).setMessage("Attribute " + stat + " exists")


  private def makeSenseVariant(senseString: String, perception: Int, sense: String) = MonsterChunk(Seq(
    Block("P#flavor", List(Key("Initiative"), Text("7"), Key("Senses"), Text(senseString), Break(),
      Key("HP"), Text("360"), Key("Bloodied"), Text("180"), Break(),
      Key("AC"), Text("23; "), Key("Fortitude"), Text("24, "), Key("Reflex"), Text("19,"), Key("Will"), Text("18"), Break(),
      Key("Resist"), Text("10 variable (1/encounter)"), Break(),
      Key("Saving Throws"), Text("+4"), Break(),
      Key("Speed"), Text("  8, climb 8  "), Break(),
      Key("Action Points"), Text("2")))),
    haveAttribute("stat:perception", perception.toString) and
      (if (sense != null)
        haveAttribute("stat:senses", sense)
      else
        notHaveAttribute("stat:senses")))

  "MonsterReader.normalizeLegacySenses" should {

    "normalize broken Senses" in {
      parseMonster(pestMinionHead, makeSenseVariant("; low-light vision", 0, "low-light vision"), secondaryStats, alignmentLine)
    }

    "normalize Senses with two modes" in {
      parseMonster(pestMinionHead,
        makeSenseVariant("Perception +10; blindsight 10, tremorsense 20", 10, "blindsight 10, tremorsense 20"),
        secondaryStats, alignmentLine)
    }

    "normalize Senses with one modes" in {
      parseMonster(pestMinionHead,
        makeSenseVariant("Perception +11; darkvision", 11, "darkvision"),
        secondaryStats, alignmentLine)
    }

    "normalize Senses with one modes, negative perception" in {
      parseMonster(pestMinionHead,
        makeSenseVariant("Perception -11; darkvision", -11, "darkvision"),
        secondaryStats, alignmentLine)
    }

    "normalize Senses with no modes" in {
      parseMonster(pestMinionHead,
        makeSenseVariant("Perception +13", 13, null),
        secondaryStats, alignmentLine)
    }
  }

  /*
   * This is a conditional test that will iterate through all cached files in a directory.
   */
  if (System.getProperty("test.basedir") != null) {
    val dir = new java.io.File(System.getProperty("test.basedir"))
    val failures = new ListBuffer[String]
    "DNDI Importer" should {
      "load everybody in " + dir.getAbsolutePath in {
        val dirIter = new DirectoryIterator(dir)
        for (file <- dirIter if (file.isFile)) {
          val failedLoad: Boolean = try {
            val xml = scala.xml.XML.loadFile(file)
            val blocks = parseBlockElements(xml.child)
            if (blocks != null && !blocks.isEmpty) {
              val monster = new MonsterReader(0).process(blocks)
              if (monster == null)
                System.err.println("Monster is null: " + file)
              monster == null
            } else {
              System.err.println("Block empty for: " + file)
              true
            }
          } catch {
            case e: Throwable =>
              System.err.println("Failed to parse: " + file + " reason: " + e.getMessage)
              true
          }
          if (failedLoad) {
            failures += ("loading failed for " + file + "\n")
          }
        }
        failures.toList must beEmpty
      }
    }
  }

  def generateMeleePower(name: String): List[BlockElement] = {
    val header = Block("P#flavor alt", List(Icon(IconType.Melee), Text(" "), Key(name), Text(" "), Icon(IconType.Separator), Text(" "), Key("Encounter")))
    val blk = Block("P#flavor", List(Text("Something happens.")))
    List(header, blk)
  }

  def generateMeleePowerAndTest(name: String, action: ActionType.Value):(Seq[BlockElement], Matcher[Power]) = (
    Seq(
      Block("P#flavor alt", List(Icon(IconType.Melee), Text(" "), Key(name), Text(" "), Icon(IconType.Separator), Text(" "), Key("Encounter"))),
      Block("P#flavor", List(Text(name + "Something happens.")))),
    likePower(
      CompletePowerDefinition(Seq(IconType.Melee), name, null, EncounterUsage()),
      action,
      StyledText.singleBlock("P", "flavor", name + "Something happens.")))

  def generateLegacyMeleePowerAndTest(name: String):(Seq[BlockElement], Matcher[Power]) = (
    Seq(
      Block("P#flavor alt", List(Icon(IconType.Melee), Key(name), Text("(standard, at-will)"), Icon(IconType.Separator), Key("Weapon"))),
      Block("P#flavor", List(Text(name + "Something happens.")))),
    likePower(
      LegacyPowerDefinition(Seq(IconType.Melee), name, "(standard, at-will)", "Weapon", null),
      null,
      StyledText.singleBlock("P", "flavor", name + "Something happens.")))

  class DirectoryIterator(dir: File) extends Iterator[File] {

    class SubIterator(subdir: File, parent: SubIterator) {
      val iter = makeFileList()

      def makeFileList(): Iterator[File] = {
        val list: List[File] =
          if (subdir.exists && subdir.isDirectory)
            subdir.list().map(x => new File(subdir, x)).toList
          else Nil
        (list ::: List(subdir)).iterator
      }

      def hasNext: Boolean = iter.hasNext || (parent != null && parent.hasNext)

      def nextRecursive: (SubIterator, File) = {
        if (iter.hasNext) {
          val file = iter.next()
          if (file.exists && file.isDirectory && file != subdir) new SubIterator(file, this).nextRecursive
          else (this, file)
        } else {
          parent.nextRecursive
        }
      }
    }

    private var si = new SubIterator(dir, null)

    def hasNext = si.hasNext

    def next(): File = {
      val (nsi, file) = si.nextRecursive
      si = nsi
      file
    }
  }
}