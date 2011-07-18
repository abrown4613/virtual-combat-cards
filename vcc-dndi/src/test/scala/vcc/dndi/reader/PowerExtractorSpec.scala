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

package vcc.dndi.reader

import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4,JUnitSuiteRunner}
import vcc.dndi.reader.Parser._

@RunWith(classOf[JUnitSuiteRunner])
class PowerExtractorTest extends JUnit4(PowerExtractorSpec)

object PowerExtractorSpec extends Specification {

  "SomePowerDefinition" should {
    "read power with name and keyword" in {
      val parts = List(Icon(IconType.Melee), Key(" Name "), Text(" Keyword "), Icon(IconType.Separator), Key("Aura"), Text(" 1"))
      parts match {
        case SomePowerDefinition(d) =>
          d match {
            case CompletePowerDefinition(icon, name, keyword, usage) =>
              name must_== "Name"
              keyword must_== "Keyword"
              usage must_== AuraUsage(1)
              icon must_== Seq(IconType.Melee)
            case _ => fail("Should be complete power definition")
          }
        case _ => fail("Should have matched")
      }
    }


    "read power with name and keyword but no usage separation, just bold" in {
      //<P class="flavor alt"> <B>Spider Burst</B> (Poison, Zone) <B></B></P>
      val parts = List(Text(" "), Key("Spider Burst"), Text(" (Poison, Zone) "), Key(""))
      parts match {
        case SomePowerDefinition(d) =>
          d must_== CompletePowerDefinition(Seq(), "Spider Burst", "(Poison, Zone)", NoUsage)
        case _ => fail("Should have matched")
      }
    }

    "read power with name, no keyword and no empty key as usage" in {
      //<P class="flavor alt"> <B>Mercurial Body</B> <B></B></P>)
      val parts = List(Text(" "), Key("Mercurial Body"), Text(" "), Key(""))
      parts match {
        case SomePowerDefinition(d) =>
          d must_== CompletePowerDefinition(Seq(), "Mercurial Body", null, NoUsage)
        case _ => fail("Should have matched")
      }
    }

    "read power with name, separator and round limited At-Will" in {
      //<P class="flavor alt"> <B>Mercurial Body</B> <B></B></P>)
      val parts = List(Text(" "), Key("Fey Light"), Text(" "), Icon(IconType.Separator), Text(" "), Key("At-Will"), Text(" (1/round)"))
      parts match {
        case SomePowerDefinition(d) =>
          d must_== CompletePowerDefinition(Seq(), "Fey Light", null, AtWillUsage(1))
        case _ => fail("Should have matched")
      }
    }

    "read power with name and no keyword" in {
      val parts = List(Icon(IconType.Melee), Key(" Name "), Icon(IconType.Separator), Key("Aura"), Text(" 1"))
      parts match {
        case SomePowerDefinition(definition) =>
          definition match {
            case CompletePowerDefinition(icon, name, keyword, usage) =>
              name must_== "Name"
              keyword must beNull
              usage must_== AuraUsage(1)
              icon must_== Seq(IconType.Melee)
            case _ => fail("Should be complete power definition")
          }
        case _ => fail("Should have matched")
      }
    }

    "handle legacy entries as such" in {
      val parts = List(Icon(IconType.Melee), Key(" Name "), Text("(standard, at-will)"), Icon(IconType.Separator), Key("Weapon"))
      parts match {
        case SomePowerDefinition(definition) =>
          definition match {
            case LegacyPowerDefinition(icon, name, actionUsage, keyword, usage) =>
              name must_== "Name"
              keyword must_== "Weapon"
              usage must beNull
              actionUsage must_== "(standard, at-will)"
              icon must_== Seq(IconType.Melee)
            case _ => fail("Should be legacy power definition")
          }
        case _ => fail("Should have matched")
      }
    }

    "read legacy power with no keywords" in {
      //<P class="flavor alt"> <B>Spider Burst</B> (Poison, Zone) <B></B></P>
      //val ts = getBlockStream(<P class="flavor alt"> <B>Goblin Tactics</B> (immediate reaction, when missed by a melee attack; at-will) </P>)
      //println("ANALYZE THIS ****: " + ts.head)

      val parts = List(Text(" "), Key("Goblin Tactics"), Text(" (immediate reaction, ...) "))
      parts match {
        case SomePowerDefinition(d) =>
          d must_== LegacyPowerDefinition(Seq(), "Goblin Tactics", "(immediate reaction, ...)", null, null)
        case _ => fail("Should have matched")
      }
    }

    "read power with encounter duration" in {
      //<P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/Z3a.gif"></IMG> <B>Darkfire</B> <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG> <B>Encounter</B></P>

      val parts = List(Icon(IconType.Range), Text(" "), Key("Darkfire"), Text(" "), Icon(IconType.Separator), Text(" "), Key("Encounter"))
      parts match {
        case SomePowerDefinition(d) =>
          d must_== CompletePowerDefinition(Seq(IconType.Range), "Darkfire", null, EncounterUsage(1))
        case _ => fail("Should have matched")
      }
    }

    //TODO: Implement fail-over strategy. If nothing fits we should get a raw block of StyledText
    "read power with recharge all in bold" in {
      //<P class="flavor alt"><IMG src="http://www.wizards.com/dnd/images/symbol/Z1a.gif"></IMG> <B>Poison Spew</B> (Poison) <IMG src="http://www.wizards.com/dnd/images/symbol/x.gif"></IMG> <B>Recharge when first bloodied</B></P>

      val parts = List(Icon(IconType.Range), Text(" "), Key("Poison Spew"), Text(" "), Icon(IconType.Separator), Text(" "), Key("Recharge when first bloodied"))
      parts match {
        case SomePowerDefinition(d) =>
          d must_== CompletePowerDefinition(Seq(IconType.Range), "Poison Spew", null, RechargeConditionalUsage("when first bloodied"))
        case _ => fail("Should have matched")
      }
    }
  }

  "WithoutParenthensis" should {
    "strip parenthesis" in {
      WithoutParenthesis.unapply("  (  test one, two 3, 4 )  ") must_== Some("test one, two 3, 4")
    }
    "keep without parenthesis" in {
      WithoutParenthesis.unapply("     test one, two 3, 4    ") must_== Some("test one, two 3, 4")
    }

    "strip one layer of parenthesis" in {
      WithoutParenthesis.unapply("  ( ( test one, two 3, 4 ))  ") must_== Some("( test one, two 3, 4 )")
    }
  }

  "SomeUsage extractor" should {

    "no additional data means no usage" in {
      SomeUsage.unapply(Nil) must_== None
    }

    "blank key means no usage" in {
      SomeUsage.unapply(List(Key(""))) must_== Some(NoUsage)
    }

    "extract an aura" in {
      val parts = List(Key("Aura"), Text(" 1"))
      SomeUsage.unapply(parts) must_== Some(AuraUsage(1))
    }

    "extract simple encounter usage" in {
      val parts = List(Key("Encounter"))
      SomeUsage.unapply(parts) must_== Some(EncounterUsage(1))
    }

    "extract simple multiple per encounter" in {
      val parts = List(Key("3 / Encounter"))
      SomeUsage.unapply(parts) must_== Some(EncounterUsage(3))

      val parts2 = List(Key("3/Encounter"))
      SomeUsage.unapply(parts2) must_== Some(EncounterUsage(3))
    }

    "handle dice recharge" in {
      val parts = List(Text(" Recharge 5 6"))
      SomeUsage.unapply(parts) must_== Some(RechargeDiceUsage(5))

      val parts2 = List(Text(" Recharge 4 5 6"))
      SomeUsage.unapply(parts2) must_== Some(RechargeDiceUsage(4))

      val parts3 = List(Text(" Recharge 6"))
      SomeUsage.unapply(parts3) must_== Some(RechargeDiceUsage(6))
    }

    "handle conditional recharge" in {
      val parts = List(Key("Recharge"), Text(" when happy"))
      SomeUsage.unapply(parts) must_== Some(RechargeConditionalUsage("when happy"))
    }

    "handle conditional recharge all in bold" in {
      val parts = List(Key("Recharge when happy"))
      SomeUsage.unapply(parts) must_== Some(RechargeConditionalUsage("when happy"))
    }

    "handle round limited at will" in {
      val parts = List(Key("At-Will"), Text(" 2 / round"))
      SomeUsage.unapply(parts) must_== Some(AtWillUsage(2))

      val parts2 = List(Key("At-Will"), Text(" 2/round"))
      SomeUsage.unapply(parts2) must_== Some(AtWillUsage(2))
    }

    "handle round limited at will in parenthesis" in {
      val parts3 = List(Key("At-Will"), Text(" (1/round)"))
      SomeUsage.unapply(parts3) must_== Some(AtWillUsage(1))
    }

    "handle unlimited at will" in {
      val parts = List(Key("At-Will"))
      SomeUsage.unapply(parts) must_== Some(AtWillUsage(0))
    }
  }

  "PowerHeaderParts" should {
    "split three way" in {
      val parts = List(Icon(IconType.Melee), Icon(IconType.Range), Key("name"), Key("2"), Icon(IconType.Separator), Key("end"), Text("game"))
      parts match {
        case PowerHeaderParts(icons, part1, part2) =>
          icons must_== Seq(IconType.Melee, IconType.Range)
          part1 must_== List(Key("name"), Key("2"))
          part2 must_== List(Key("end"), Text("game"))
        case _ => fail("Should match")
      }
    }

    "split with no separator" in {
      val parts = List(Icon(IconType.Melee), Icon(IconType.Range), Key("name"), Key("2"), Key("end"), Text("game"))
      parts match {
        case PowerHeaderParts(icons, part1, part2) =>
          icons must_== Seq(IconType.Melee, IconType.Range)
          part1 must_== List(Key("name"), Key("2"), Key("end"), Text("game"))
          part2 must_== List()
        case _ => fail("Should match")
      }
    }
    "split with no icons" in {
      val parts = List(Key("name"), Key("2"), Icon(IconType.Separator), Key("end"), Text("game"))
      parts match {
        case PowerHeaderParts(icons, part1, part2) =>
          icons must_== Seq()
          part1 must_== List(Key("name"), Key("2"))
          part2 must_== List(Key("end"), Text("game"))
        case _ => fail("Should match")
      }
    }

    "transform empty Key from MM3 entris into a fake separation" in {
      //<P class="flavor alt"> <B>Spider Burst</B> (Poison, Zone) <B></B></P>
      val parts = List(Text(" "), Key("name"), Text("end"), Key(""))
      parts match {
        case PowerHeaderParts(icons, part1, part2) =>
          icons must_== Seq()
          part1 must_== List(Key("name"), Text("end"))
          part2 must_== List(Key(""))
        case _ => fail("Should match")
      }
    }


    "split after cleaning bad power headers" in {
      val parts = List(Key("name"), Key("2"), Icon(IconType.Separator), Key("end"), Text("game"), Break(), Text("now"))
      parts match {
        case PowerHeaderParts(icons, part1, part2) =>
          icons must_== Seq()
          part1 must_== List(Key("name"), Key("2"))
          part2 must_== List(Key("end"), Text("game now"))
        case _ => fail("Should match")
      }
    }
  }
  "SectionActionType extractor" should {

    "handle mixed case and spaces" in {
      SectionActionType.unapply(List(Text(" StAnDard ActiOns "))) must_== Some(ActionType.Standard)
    }

    "handle all defined types" in {
      ActionType.values.foreach(at => SectionActionType.unapply(List(Text((at.toString + "s").toUpperCase))) must_== Some(at))
    }
  }

  "ActionType extractor" should {
    "handle all defined types" in {
      ActionType.values.foreach(at => ActionType.unapply(at.toString.toUpperCase) must_== Some(at))
    }

  }
}