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
package vcc.dnd4e.tracker.ruling

import org.specs2.SpecificationWithJUnit
import vcc.dnd4e.tracker.transition.DamageCommand
import vcc.dnd4e.tracker.event.{EventSourceSampleEvents}
import vcc.dnd4e.tracker.common.{EffectID, CombatState}

class OngoingDamageRulingTest extends SpecificationWithJUnit with EventSourceSampleEvents {
  def is =
    "OngoingDamageRuling".title ^
      "have answer" ! e1 ^
      "have a user prompt" ! e4 ^
      "produce command no command when zero" ! e2 ^
      "produce damage command when set to greater than 0" ! e3 ^
      end

  private val eid = EffectID(combA, 1)
  private val noDamageOngoingRuling = OngoingDamageRuling(OngoingDamage.CausedBy(eid), Some(OngoingDamage.DamageToApply(0)))
  private val damageOngoingRuling = OngoingDamageRuling(OngoingDamage.CausedBy(eid), Some(OngoingDamage.DamageToApply(7)))

  val state = CombatState.empty.transitionWith(List(evtAddCombA, makeBadEndOfEncounterEffect(combA, combB, "ongoing 5 fire")))

  private def e1 = {
    noDamageOngoingRuling.hasDecision must beTrue
  }

  private def e2 = {
    noDamageOngoingRuling.generateCommands(state) must_== Nil
  }

  private def e3 = {
    damageOngoingRuling.generateCommands(state) must_== List(DamageCommand(combA, 7))
  }

  private def e4 = {
    noDamageOngoingRuling.userPrompt(state) must_== "Fighter [A] affected by: ongoing 5 fire"
  }
}