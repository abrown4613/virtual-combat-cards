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
import vcc.dnd4e.tracker.transition.CancelEffectCommand
import vcc.tracker.Ruling
import vcc.dnd4e.tracker.common.{CombatState, EffectID}

class SaveRulingTest extends RulingAcceptance("SaveRuling") with EventSourceSampleEvents {
  private val eid = EffectID(combA, 1)
  private val savedRuling = SaveRuling(eid, Some(SaveRulingResult.Saved))
  private val failedRuling = SaveRuling(eid, Some(SaveRulingResult.Failed))

  protected val rulingWithAnswer: Ruling[CombatState, _, _] = savedRuling
  protected val rulingWithoutAnswer: Ruling[CombatState, _, _] = SaveRuling(eid, None)
  protected val userPromptMessage: String = "Fighter [A] must make a saving throws against: bad"
  protected val state = emptyState.transitionWith(List(evtAddCombA, makeBadEndOfEncounterEffect(combA, combB, "bad")))

  def buildCases =
    "produce answer" ! (savedRuling.generateCommands(state) must_== List(CancelEffectCommand(eid))) ^
      "produce no answer if not saved" ! (failedRuling.generateCommands(state) must_== Nil) ^
      end
}