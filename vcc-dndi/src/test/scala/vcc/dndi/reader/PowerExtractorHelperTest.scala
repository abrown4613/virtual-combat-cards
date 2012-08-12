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

import vcc.dndi.reader.Parser._
import org.specs2.mutable.SpecificationWithJUnit

class PowerExtractorHelperTest extends SpecificationWithJUnit {
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
      SomeUsage.unapply(parts) must_== Some(AuraUsage("1"))
    }

    "extract simple encounter usage" in {
      val parts = List(Key("Encounter"))
      SomeUsage.unapply(parts) must_== Some(EncounterUsage())
    }

    "extract simple multiple per encounter" in {
      val parts = List(Key("2 / Encounter"))
      SomeUsage.unapply(parts) must_== Some(EncounterUsage("2/Encounter"))

      val parts2 = List(Key("3/Encounter"))
      SomeUsage.unapply(parts2) must_== Some(EncounterUsage("3/Encounter"))
    }

    "extract simple multiple per encounter" in {
      val parts = List(Key("Encounter"), Text(" some detail"))
      SomeUsage.unapply(parts) must_== Some(EncounterUsage("some detail"))
    }

    "handle dice recharge" in {
      val parts = List(Text(" Recharge 5 6"))
      SomeUsage.unapply(parts) must_== Some(RechargeDiceUsage(5))

      val parts2 = List(Text(" Recharge 4 5 6"))
      SomeUsage.unapply(parts2) must_== Some(RechargeDiceUsage(4))

      val parts3 = List(Text(" Recharge 6"))
      SomeUsage.unapply(parts3) must_== Some(RechargeDiceUsage(6))

      val parts4 = List(Text("Recharge 4"))
      SomeUsage.unapply(parts4) must_== Some(RechargeDiceUsage(4))
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
      SomeUsage.unapply(parts) must_== Some(AtWillUsage("2/round"))

      val parts2 = List(Key("At-Will"), Text(" 2/round"))
      SomeUsage.unapply(parts2) must_== Some(AtWillUsage("2/round"))
    }

    "handle round limited at will in parenthesis" in {
      val parts3 = List(Key("At-Will"), Text(" (1/round)"))
      SomeUsage.unapply(parts3) must_== Some(AtWillUsage("1/round"))
    }

    "handle unlimited at will" in {
      val parts = List(Key("At-Will"))
      SomeUsage.unapply(parts) must_== Some(AtWillUsage())
    }

    "handle at will with text" in {
      val parts = List(Key("At-Will"), Text(" polymorph "))
      SomeUsage.unapply(parts) must_== Some(AtWillUsage("polymorph"))
    }

    "handle at will with text in parenthesis" in {
      val parts = List(Key("At-Will"), Text(" (polymorph )"))
      SomeUsage.unapply(parts) must_== Some(AtWillUsage("polymorph"))
    }
  }

  "SectionActionType extractor" should {

    "handle mixed case and spaces" in {
      SectionActionType.unapply(List(Text(" StAnDard ActiOns "))) must_== Some(ActionType.Standard)
    }

    "handle all defined types in plural" in {
      for(at <- ActionType.values.toSeq) yield {
        SectionActionType.unapply(List(Text((at.toString + "s").toUpperCase))) must_== Some(at)
      }
    }
  }

  "ActionType extractor" should {
    "handle all defined types" in {
      for(at <- ActionType.values.toSeq) yield {
        ActionType.unapply(at.toString.toUpperCase) must_== Some(at)
      }
    }
  }
}