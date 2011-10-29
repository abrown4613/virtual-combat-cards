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

import vcc.dnd4e.tracker.event.EventSourceSampleEvents
import vcc.dnd4e.tracker.transition.{UpdateEffectConditionCommand, CancelEffectCommand}
import vcc.tracker.Ruling
import vcc.dnd4e.tracker.common.{CombatState, Effect, EffectID}

class SaveSpecialRulingTest extends RulingAcceptance("SaveSpecialRuling") with EventSourceSampleEvents {

  private val eid = EffectID(combA, 1)
  private val savedRuling = SaveSpecialRuling(eid, Some(SaveSpecialRulingResult.Saved))
  private val changedRuling = SaveSpecialRuling(eid, Some(SaveSpecialRulingResult.Changed("worst")))

  protected val state = emptyState.transitionWith(List(evtAddCombA, makeBadEndOfEncounterEffect(combA, combB, "bad -> worst")))
  protected val rulingWithAnswer: Ruling[CombatState, _, _, _] = savedRuling
  protected val rulingWithoutAnswer: Ruling[CombatState, _, _, _] = SaveSpecialRuling(eid, None)
  protected val userPromptMessage: String = "Fighter [A] must make a saving throws against: bad -> worst"

  def buildCases =
    "produce on saved" ! (savedRuling.generateCommands(state) must_== List(CancelEffectCommand(eid))) ^
      "produce on changed" ! (changedRuling.generateCommands(state) must_== List(UpdateEffectConditionCommand(eid, Effect.Condition.Generic("worst", false)))) ^
      "create ruling from effect" ! e5 ^
      end

  private def e5 = {
    val eid = EffectID(combA, 1)
    val effect = state.combatant(combA).effects.find(eid).get

    SaveSpecialRuling.rulingFromEffect(effect) must_== SaveSpecialRuling(eid, None)
  }
}