//$Id$
/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.domain.dndi

import org.specs._

import Parser._

object MonsterCaptureSpec extends Specification {

  var monster:Monster = null
  var reader:BlockReader = null

  val ctx = beforeContext {
    monster = new Monster(0)
    reader = new MonsterBuilder(monster)
  }

  "MonsterImporter " ->- (ctx) should  {
    "extract name,xp,level and type from header" in {
      val head = HeaderBlock("H1#monster",List(("name","Big bird"), ("type","Large elemental beast (avian)"), ("level","Level 8 Solo Brute"), ("xp","XP 1750")))
      reader.processBlock(head) must beTrue
      monster("NAME") must_== Some("Big bird")
      monster("XP") must_== Some("1750")
      monster("LEVEL") must_== Some("8")
      monster("TYPE") must_== Some("Large elemental beast (avian)")
      monster("ROLE") must_== Some("Solo Brute")
    }
    "extract name,xp,level and type from Minion header" in {
      val head = HeaderBlock("H1#monster",List(("name","Big bird"), ("type","Large elemental beast (avian)"), ("level","Level 8 Minion"), ("xp","XP 1750")))
      reader.processBlock(head) must beTrue
      monster("NAME") must_== Some("Big bird")
      monster("XP") must_== Some("1750")
      monster("ROLE") must_== Some("No Role")
    }
    "capture block with alignment" in {
      val align = Block("P#flavor alt",List(Key("Alignment"), Text("Evil"), Key("Languages"), Text("Common, Supernal"), Break(),
                                          Key("Str"), Text("20 (+9)"), Key("Dex"), Text("11 (+4)"), Key("Wis"), Text("16 (+7)"), Break(),
                                          Key("Con"), Text("10 (+4)"), Key("Int"), Text("13 (+5)"), Key("Cha"), Text("12 (+5)")))
      reader.processBlock(align) must beTrue
      monster("ALIGNMENT") must_== Some("Evil")
      monster("LANGUAGES") must_== Some("Common, Supernal")
      monster("STR") must_== Some("20 (+9)")
    }
    "capture equipment" in {
      val equip = Block("P#flavor alt",List(Key("Equipment"), Text("yellow"),Break(),Text("feathers")))
      reader.processBlock(equip) must beTrue
      monster("EQUIPMENT") must_== Some("yellow feathers")
    }

    "capture primary stat block" in {
      val stb = Block("P#flavor",List(Key("Initiative"), Text("7"), Key("Senses"), Text("Perception +12; low-light vision"), Break(),
                                    Key("Aura name 1"), Text("(Fire) aura 1; Some long description about the aura."), Break(),
                                    Key("Aura name 2"), Text("(Thunder) aura 2; Some long description about the aura."), Break(),
                                    Key("HP"), Text("360"), Key("Bloodied"), Text("180"), Break(), Key("AC"), Text("23"), Key("Fortitude"), Text("24"), Key("Reflex"), Text("19"), Key("Will"), Text("19"), Break(),
                                    Key("Resist"), Text("10 variable (1/encounter)"), Break(),
                                    Key("Saving Throws"), Text("4"), Break(),
                                    Key("Speed"), Text("8, climb 8"), Break(),
                                    Key("Action Points"), Text("2")))
      reader.processBlock(stb) must beTrue
      monster("INITIATIVE") must_== Some("7")
      monster("HP") must_== Some("360")
      monster("RESIST") must_== Some("10 variable (1/encounter)")
      monster("ACTION POINTS") must_== Some("2")
      monster.auras mustNot beEmpty
      monster.auras must contain(Monster.Aura("Aura name 1","(Fire) aura 1; Some long description about the aura."))
      monster.auras must contain(Monster.Aura("Aura name 2","(Thunder) aura 2; Some long description about the aura."))
    }
    "capture minion primary stat block" in {
      val xstb =(<P class="flavor"><B>Initiative</B> +7 <B>Senses</B> Perception +5; low-light vision<BR></BR><B>HP</B> 1; a missed attack never damages a minion.<BR></BR><B>AC</B> 22; <B>Fortitude</B> 20, <B>Reflex</B> 18, <B>Will</B> 18<BR></BR><B>Speed</B> 7</P>)
      reader.processBlock(Parser.parseBlockElement(xstb,true)) must beTrue
      monster("INITIATIVE") must_== Some("7")
      monster("HP") must_== Some("1")
      monster("AC") must_== Some("22")
      monster("SPEED") must_== Some("7")
    }

    "capture power with Icon Key Text Separator Key " in {
      val phl = (<P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/S2.gif"></IMG><B>Mace</B> (standard, at-will) <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG><B>Arcane, Weapon</B></P>)
      val pdl = (<P class="flavorIndent">+12 vs AC; 1d8 + 4 damage, and the next of big allies to attack the target gains +2 to its attack roll..</P>)
      reader.processBlock(Parser.parseBlockElement(phl,true)) must beTrue
      reader.processBlock(Parser.parseBlockElement(pdl,true)) must beTrue
      monster.powers mustNot beEmpty
      val p = monster.powers(0)
      p.name must_== "Mace"
      p.icon must contain(IconType.MeleeBasic)
      p.description must_== "+12 vs AC; 1d8 + 4 damage, and the next of big allies to attack the target gains +2 to its attack roll.."
      p.action must_== "(standard, at-will)"
      p.keywords must_== "Arcane, Weapon"

    }
    "capture power with Key Text Separator Key " in {
      val phl = (<P class="flavor alt"><B>Rod Arc</B> (standard, at-will) <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif" /><B>Arcane, Implement, Lightning</B></P>)
      val pdl = (<P class="flavorIndent">Birdy ignores the target’s cover; 10;  </P>)
      reader.processBlock(Parser.parseBlockElement(phl,true)) must beTrue
      reader.processBlock(Parser.parseBlockElement(pdl,true)) must beTrue
      monster.powers mustNot beEmpty
      val p = monster.powers(0)
      p.name must_== "Rod Arc"
      p.icon must beEmpty
      p.description must_== "Birdy ignores the target’s cover; 10" // Note: Trim will remove the last ;
      p.action must_== "(standard, at-will)"
      p.keywords must_== "Arcane, Implement, Lightning"
    }
    "capture power with Icon Key Text (and recharge)" in {
      val phl = <P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/Z4a.gif" /><B>Shard Orb</B> (standard, recharge <IMG src="http://www.wizards.com/dnd/images/symbol/4a.gif" /><IMG src="http://www.wizards.com/dnd/images/symbol/5a.gif" /><IMG src="http://www.wizards.com/dnd/images/symbol/6a.gif" />)</P>
      val pdl = <P class="flavorIndent">Area burst 1 within 10; targets enemies; +10 vs Fortitude</P>
      reader.processBlock(Parser.parseBlockElement(phl,true)) must beTrue
      reader.processBlock(Parser.parseBlockElement(pdl,true)) must beTrue
      val p = monster.powers(0)
      p.name must_== "Shard Orb"
      p.icon must contain(IconType.Area)
      p.description must_== "Area burst 1 within 10; targets enemies; +10 vs Fortitude"
      p.action must_== "(standard, recharge 4 5 6 )"
      p.keywords must beNull
    }
    "capture power with just a Key" in {
      val phl = (<P class="flavor alt"><B>Simple power</B></P>)
      val pdl = (<P class="flavorIndent">Description for the power.</P>)
      reader.processBlock(Parser.parseBlockElement(phl,true)) must beTrue
      reader.processBlock(Parser.parseBlockElement(pdl,true)) must beTrue
      monster.powers mustNot beEmpty
      val p = monster.powers(0)
      p.name must_== "Simple power"
      p.icon must beEmpty
      p.description must_== "Description for the power."
      p.action must beNull
      p.keywords must beNull
    }
    "capture power with Key and Text " in {
      val phl = <P class="flavor alt"><B>Goblin Tactics</B> (immediate reaction, when missed by a melee<BR/>attack) </P>
      val pdl = <P class="flavorIndent">Birdy shifts 1 square.</P>
      reader.processBlock(Parser.parseBlockElement(phl,true)) must beTrue
      reader.processBlock(Parser.parseBlockElement(pdl,true)) must beTrue
      monster.powers mustNot beEmpty
      val p = monster.powers(0)
      p.name must_== "Goblin Tactics"
      p.icon must beEmpty
      p.description must_== "Birdy shifts 1 square."
      p.action must_== "(immediate reaction, when missed by a melee attack)"
      p.keywords must beNull
    }

    "capture secondary attack information" in {
      val parts = List(
        (<P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/Z1a.gif" /><B>Brutal Juju</B> (standard, at-will) <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif" /><B>Thunder</B></P>),
        (<P class="flavorIndent">Bad juju.</P>),
        (<P class="flavor"><I>Secondary Attack (poison)</I></P>),
      	(<P class="flavorIndent">Really bad juju.</P>))
      parts.foreach{ p => reader.processBlock(Parser.parseBlockElement(p,true)) must beTrue}
      monster.powers mustNot beEmpty
      val p = monster.powers(0)
      p.name must_== "Brutal Juju"
      p.icon must contain(IconType.Close)
      p.description must_== "Bad juju."
      p.action must_== "(standard, at-will)"
      p.keywords must_== "Thunder"
      p.supplement mustNot beEmpty
      p.supplement.length must_== 1
      p.supplement(0).emphasis must_== "Secondary Attack (poison)"
      p.supplement(0).text must_== "Really bad juju."
    }

    "capture secondary attack information no keyword" in {
      val parts = List(
        (<P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/Z1a.gif" /><B>Brutal Juju</B> (standard, at-will) <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif" /><B>Thunder</B></P>),
        (<P class="flavorIndent">Bad juju.</P>),
        (<P class="flavor"><I>Secondary Attack </I></P>),
      	(<P class="flavorIndent">Really bad juju.</P>))
      parts.foreach{ p => reader.processBlock(Parser.parseBlockElement(p,true)) must beTrue}
      monster.powers mustNot beEmpty
      val p = monster.powers(0)
      p.name must_== "Brutal Juju"
      p.icon must contain(IconType.Close)
      p.description must_== "Bad juju."
      p.action must_== "(standard, at-will)"
      p.keywords must_== "Thunder"
      p.supplement mustNot beEmpty
      p.supplement.length must_== 1
      p.supplement(0).emphasis must_== "Secondary Attack"
      p.supplement(0).text must_== "Really bad juju."
    }

    "capture monster with italic blocks in power description" in {
      val parts = List(
        (<P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/Z1a.gif" /><B>Brutal Juju</B> (standard, at-will) <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif" /><B>Thunder</B></P>),
        (<P class="flavorIndent">Bad juju.</P>),
        (<P class="flavor"><I>Magic beak</I></P>),
      	(<P class="flavorIndent">Really bad juju.</P>),
        (<P class="flavor"><I>Dancing feet</I></P>),
        (<P class="flavorIndent">Really bad jarjar.</P>))
      parts.foreach{ p => reader.processBlock(Parser.parseBlockElement(p,true)) must beTrue}
      monster.powers mustNot beEmpty
      val p = monster.powers(0)
      p.name must_== "Brutal Juju"
      p.icon must contain(IconType.Close)
      p.description must_== "Bad juju."
      p.action must_== "(standard, at-will)"
      p.keywords must_== "Thunder"
      p.supplement mustNot beEmpty
      p.supplement.length must_== 2
      p.supplement(0).emphasis must_== "Magic beak"
      p.supplement(0).text must_== "Really bad juju."
      p.supplement(1).emphasis must_== "Dancing feet"
      p.supplement(1).text must_== "Really bad jarjar."
    }

    "capture power with two icons" in {
      val blk = Block("P#flavor alt",List(Icon(IconType.MeleeBasic), Icon(IconType.RangeBasic), Key("Handaxe"), Text("(standard, at-will)"), Icon(IconType.Separator), Key("Weapon")))
      reader.processBlock(blk) must beTrue
      monster.powers mustNot beEmpty
      val p = monster.powers(0)
      p.name must_== "Handaxe"
      p.icon mustNot beEmpty
      p.icon must contain(IconType.MeleeBasic)
      p.icon must contain(IconType.RangeBasic)
    }

    "capture description" in {
      val blk = Block("P#flavor",List(Key("Description"), Text("All gnolls"), Break(), Text("are ducks.")))
      reader.processBlock(blk) must beTrue
      monster("DESCRIPTION") must_== Some("All gnolls\nare ducks.")
    }

    "ignore trailing items" in {
      val phl= (<BR/>)
      reader.processBlock(Parser.parseBlockElement(phl,true)) must beTrue
    }

    "add publish to comment" in {
      val xml = (<P><I>Published in <A href="http://www.wizards.com/default.asp?x=dnd/dutoc/169" target="_new">Dungeon Magazine 169</A>.</I></P>)
      reader.processBlock(Parser.parseBlockElement(xml,true)) must beTrue
      monster("COMMENT") must_== Some("Published in Dungeon Magazine 169.")
    }
  }

  if(System.getProperty("test.basedir")!= null) {
    val dir = new java.io.File(System.getProperty("test.basedir"))
    "Monster Importer" ->- (ctx) should {
      val dirIter = new vcc.util.DirectoryIterator(dir,false)
      for(file <- dirIter if(file.isFile)) {
         "load "+file in {
        	val xml = scala.xml.XML.loadFile(file)
        	val blocks = parseBlockElements(xml.child,true)
        	blocks must notBeNull
        	blocks must notBeEmpty
            for(b<-blocks) {
              reader.processBlock(b) must beTrue
            }
         }
      }
    }
  }
}
