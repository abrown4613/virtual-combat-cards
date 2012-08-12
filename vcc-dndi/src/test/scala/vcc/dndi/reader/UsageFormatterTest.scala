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

import xml.{Text, Node, NodeSeq}
import org.specs2.mutable.SpecificationWithJUnit

class UsageFormatterTest extends SpecificationWithJUnit {

  def nodeSeq(node: Node*): NodeSeq = node

  final val atWillUsageXML = nodeSeq(<img src="x.gif"/>, Text(" "), <b>At-Will</b>)

  "UsageFormatter" should {

    "return Aura" in {
      UsageFormatter.format(AuraUsage("2")) must_== nodeSeq(<img src="x.gif"/>, Text(" "), <b>Aura</b>, Text(" 2"))
    }

    "return unlimited At-Will" in {
      UsageFormatter.format(AtWillUsage()) must_== atWillUsageXML
    }

    "return limited At-Will" in {
      UsageFormatter.format(AtWillUsage("2/round")) must_== nodeSeq(<img src="x.gif"/>, Text(" "), <b>At-Will</b>, Text(" 2/round"))
    }

    "return unlimited Encounter" in {
      UsageFormatter.format(EncounterUsage()) must_== nodeSeq(<img src="x.gif"/>, Text(" "), <b>Encounter</b>)
    }

    "return limited Encounter" in {
      UsageFormatter.format(EncounterUsage("2/Encounter")) must_== nodeSeq(<img src="x.gif"/>, Text(" "), <b>Encounter</b>, Text(" 2/Encounter"))
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