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

import vcc.dnd4e.tracker.command.DamageCommand
import vcc.dnd4e.tracker.event.{EventSourceSampleEvents}
import vcc.dnd4e.tracker.common.{EffectID, CombatState}
import vcc.tracker.Ruling

class OngoingDamageRulingTest extends RulingAcceptance[CombatState]("OngoingDamageRuling") with EventSourceSampleEvents {

  private val eid = EffectID(combA, 1)
  private val noDamageOngoingRuling = OngoingDamageRuling(eid, Some(0))
  private val damageOngoingRuling = OngoingDamageRuling(eid, Some(7))

  protected val state = CombatState.empty.transitionWith(List(evtAddCombA, makeBadEndOfEncounterEffect(combA, combB, "ongoing 5 fire")))
  protected val rulingWithAnswer: Ruling[CombatState, _, _] = noDamageOngoingRuling
  protected val rulingWithoutAnswer: Ruling[CombatState, _, _] = OngoingDamageRuling(eid, None)
  protected val userPromptMessage: String = "Fighter [A] affected by: ongoing 5 fire"

  def buildCases =
    "produce command no command when zero" !
      (noDamageOngoingRuling.generateCommands(state) must_== Nil) ^
      "produce damage command when set to greater than 0" !
        (damageOngoingRuling.generateCommands(state) must_== List(DamageCommand(combA, 7)))
}