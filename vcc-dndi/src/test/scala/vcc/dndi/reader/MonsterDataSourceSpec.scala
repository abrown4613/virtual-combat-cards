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
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import Parser.IconType
import xml.{Text, Node, NodeSeq}
import vcc.infra.text.StyledText
import org.specs.mock.Mockito
import vcc.infra.xtemplate.TemplateDataSource

@RunWith(classOf[JUnitSuiteRunner])
class MonsterDataSourceTest extends JUnit4(MonsterDataSourceSpec)

object MonsterDataSourceSpec extends Specification with Mockito {

  var power: Power = null
  final val desc = StyledText.singleBlock("P", "flavor", "Do something")
  final val descXML = <p class='flavor'>Do something</p>
  final val atWillUsageXML = nodeSeq(<img src="x.gif"/>, Text(" "), <b>At-Will</b>)

  def nodeSeq(node: Node*): NodeSeq = node

  def commonPowerdefinition = {
    "provide correct image" in {
      power.templateInlineXML("iconset-short") must_== nodeSeq(<img src="z2a.gif"/>)
    }
    "provide correct name" in {
      power.templateVariable("name") must_== Some("name")
    }
    "provide correct keyword" in {
      power.templateVariable("keyword") must_== Some("keyword")
    }
    "provide correct usage" in {
      power.templateInlineXML("usage") must_== atWillUsageXML
    }
    "provide correct description" in {
      power.templateInlineXML("description") must_== nodeSeq(descXML)
    }

    "provide correct action" in {
      power.templateVariable("action") must_== Some(ActionType.Standard.toString)
    }
  }

  "CompletePowerDefinition" ->- (beforeContext {
    power = Power(CompletePowerDefinition(Seq(IconType.Melee), "name", "keyword", AtWillUsage(0)),
      ActionType.Standard, desc)
  }) should {
    commonPowerdefinition
  }

  "LegacyPowerDefinition" ->- (beforeContext {
    power = Power(LegacyPowerDefinition(Seq(IconType.Melee), "name", "(standart; at-will)", "keyword", AtWillUsage(0)),
      ActionType.Standard, desc)
  }) should {
    commonPowerdefinition
    "return usage-action" in {
      power.templateVariable("action-usage") must_== Some("(standart; at-will)")
    }
    "usage-action must not return null" in {
      val pd = LegacyPowerDefinition(Seq(IconType.Melee), "name", null, "keyword", AtWillUsage(0))
      pd.templateVariable("action-usage") must_== None
    }
  }

  "Monster as DataSource" should {

    val mockLegacy = List(mock[Power])
    val mockPowerGroup = List(mock[Power])
    val mockAttribute = mock[Map[String, String]]
    val monster = new Monster(10, mockAttribute, mockLegacy, Map(ActionType.Standard -> mockPowerGroup))

    "return an attribute" in {
      //Should be case insensitive
      mockAttribute.get("foo") returns Some("bar")
      monster.templateVariable("FoO") must_== Some("bar")
      there was one(mockAttribute).get("foo")
    }

    "return legacy group" in {
      monster.templateGroup("legacy") must beEqualTo(mockLegacy.asInstanceOf[List[TemplateDataSource]])
    }

    "return action group by action name group" in {
      ActionType.unapply("staNdard action") must_== Some(ActionType.Standard) // Safety for sanity
      monster.templateGroup("staNdard action") must beEqual[List[TemplateDataSource]](mockPowerGroup)
    }
  }


  "UsageFormatter" should {

    "return Aura" in {
      UsageFormatter.format(AuraUsage(2)) must_== nodeSeq(<img src="x.gif"/>, Text(" "), <b>Aura</b>, Text(" 2"))
    }

    "return unlimited At-Will" in {
      UsageFormatter.format(AtWillUsage(0)) must_== atWillUsageXML
    }

    "return limited At-Will" in {
      UsageFormatter.format(AtWillUsage(2)) must_== nodeSeq(<img src="x.gif"/>, Text(" "), <b>At-Will</b>, Text(" 2/round"))
    }

    "return unlimited Encounter" in {
      UsageFormatter.format(EncounterUsage(0)) must_== nodeSeq(<img src="x.gif"/>, Text(" "), <b>Encounter</b>)
    }

    "return limited Encounter" in {
      UsageFormatter.format(EncounterUsage(2)) must_== nodeSeq(<img src="x.gif"/>, Text(" "), <b>Encounter</b>, Text(" 2/encounter"))
    }

    "return Daily" in {
      UsageFormatter.format(DailyUsage) must_== nodeSeq(<img src="x.gif"/>, Text(" "), <b>Daily</b>)
    }

    "return No Usage" in {
      UsageFormatter.format(NoUsage) must_== <b></b>
    }

    "return conditional recharge" in {
      UsageFormatter.format(RechargeConditionalUsage("when happy")) must_== nodeSeq(<img src="x.gif"/>, Text(" "), <b>Recharge</b>, Text(" when happy"))
    }

    "return recharge dice 2" in {
      UsageFormatter.format(RechargeDiceUsage(2)) must_== nodeSeq(<img src="x.gif"/>, Text(" "), <b>Recharge</b>, Text(" "),
          <img src="2a.gif"/>, <img src="3a.gif"/>, <img src="4a.gif"/>,
          <img src="5a.gif"/>, <img src="6a.gif"/>)
    }

    "return recharge dice 5" in {
      UsageFormatter.format(RechargeDiceUsage(5)) must_== nodeSeq(<img src="x.gif"/>, Text(" "), <b>Recharge</b>, Text(" "),
          <img src="5a.gif"/>, <img src="6a.gif"/>)
    }

  }

}
