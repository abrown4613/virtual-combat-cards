/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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

import Parser.IconType
import xml.{Text, Node, NodeSeq}
import vcc.infra.text.StyledText
import org.specs2.SpecificationWithJUnit

class PowerDataSourceTest extends SpecificationWithJUnit {

  final private val desc = StyledText.singleBlock("P", "flavor", "Do something")
  final private val descXML = <p class='flavor'>Do something</p>
  final private val atWillUsageXML = nodeSeq(<img src="x.gif"/>, Text(" "), <b>At-Will</b>)

  def nodeSeq(node: Node*): NodeSeq = node

  def commonPowerdefinition(power: Power) = {
    Seq(
      "provide correct image" ! {
        power.templateInlineXML("iconset-short") must_== nodeSeq(<img src="z2a.gif"/>)
      },
      "provide correct name" ! {
        power.templateVariable("name") must_== Some("name")
      },
      "provide correct keyword" ! {
        power.templateVariable("keyword") must_== Some("keyword")
      },
      "provide correct usage" ! {
        power.templateInlineXML("usage") must_== atWillUsageXML
      },
      "provide correct description" ! {
        power.templateInlineXML("description") must_== nodeSeq(descXML)
      },
      "provide correct action" ! {
        power.templateVariable("action") must_== Some(ActionType.Standard.toString)
      })
  }

  private val completePower = Power(CompletePowerDefinition(Seq(IconType.Melee), "name", "keyword", AtWillUsage(0)),
    ActionType.Standard, desc)

  private val legacyPower = Power(LegacyPowerDefinition(Seq(IconType.Melee), "name", "(standart; at-will)", "keyword", AtWillUsage(0)),
    ActionType.Standard, desc)

  def is = "PowerDataSource" ^
    "CompletePowerDefinition should" ^ commonPowerdefinition(completePower) ^
    end ^
    "LegacyPowerDefinition should" ^ commonPowerdefinition(legacyPower) ^
    "return usage-action" ! {
      legacyPower.templateVariable("action-usage") must_== Some("(standart; at-will)")
    } ^
    "usage-action must not return null" ! {
      val pd = LegacyPowerDefinition(Seq(IconType.Melee), "name", null, "keyword", AtWillUsage(0))
      pd.templateVariable("action-usage") must_== None
    } ^ end
}
