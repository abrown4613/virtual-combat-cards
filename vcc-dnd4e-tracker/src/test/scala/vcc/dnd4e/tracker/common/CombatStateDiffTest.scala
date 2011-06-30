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
//$Id$
package vcc.dnd4e.tracker.common

import org.specs2.SpecificationWithJUnit
import org.specs2.mock.Mockito

class CombatStateDiffTest extends SpecificationWithJUnit with Mockito with SampleStateData {
  def is =

    "Combatant diference should" ^
      "show change in definition" ! combatant().diffDef ^
      "show change in health" ! combatant().diffHealth ^
      "show change in effects" ! combatant().diffEffect ^
      "show change in comment" ! combatant().diffComment ^
      endp ^
      "Roster diference should" ^
      "return difference of all comabants in both rosters" ! e1 ^
      endp

  case class combatant() extends Mockito {
    val mockDef = mock[CombatantRosterDefinition]
    mockDef.cid returns combA
    val mockHealth = mock[HealthTracker]
    val mockEffect = mock[EffectList]
    val c1 = Combatant(mockDef, "comment1", mockHealth, mockEffect)

    def diffDef = {
      val md = mock[CombatantRosterDefinition]
      val c2 = c1.copy(definition = md)
      c1.diff(c2) must_== Set(CombatantDiff(combA, mockDef, md))
    }

    def diffHealth = {
      val m = mock[HealthTracker]
      val c2 = c1.copy(health = m)
      c1.diff(c2) must_== Set(CombatantDiff(combA, mockHealth, m))
    }

    def diffEffect = {
      val m = mock[EffectList]
      val c2 = c1.copy(effects = m)
      c1.diff(c2) must_== Set(CombatantDiff(combA, mockEffect, m))
    }

    def diffComment = {
      val c2 = c1.copy(comment = "comment2")
      c1.diff(c2) must_== Set(CombatantCommentDiff(combA, "comment1", "comment2"))
    }
  }

  def e1 = {

    val mockA = mock[Combatant]
    val mockB = mock[Combatant]
    val mock1 = mock[Combatant]
    val mock2 = mock[Combatant]
    val diff1 = mock[CombatantCommentDiff]
    val diff2 = mock[CombatantCommentDiff]

    val r1 = Roster(Combatant.RosterFactory, Map(combA -> mockA, combB -> mockB, comb1 -> mock1))
    val r2 = Roster(Combatant.RosterFactory, Map(comb2 -> mock2, combB -> mockB, comb1 -> mock1))

    mockB.diff(mockB) returns Set(diff1)
    mock1.diff(mock1) returns Set(diff2)

    (r1.combatantDiff(r2) must_== Set(diff1, diff2)) and
      (there was one(mockB).diff(mockB)) and
      (there was one(mock1).diff(mock1)) and
      (there was no(mockA).diff(mockA)) and
      (there was no(mock2).diff(mock2))
  }

}