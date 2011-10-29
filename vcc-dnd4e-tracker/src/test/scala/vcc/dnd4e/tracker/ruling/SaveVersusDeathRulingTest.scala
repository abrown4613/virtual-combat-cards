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

import vcc.dnd4e.tracker.transition.{HealCommand, FailDeathSaveCommand}
import vcc.dnd4e.tracker.event.{ApplyDamageEvent, EventSourceSampleEvents}
import vcc.dnd4e.tracker.common.{CombatState}
import vcc.tracker.Ruling

class SaveVersusDeathRulingTest extends RulingAcceptance("SaveVersusDeathRuling") with EventSourceSampleEvents {
  private val savedRuling = SaveVersusDeathRuling(combA, Some(SaveVersusDeathResult.Saved))
  private val savedAndHealRuling = SaveVersusDeathRuling(combA, Some(SaveVersusDeathResult.SaveAndHeal))
  private val failedRuling = SaveVersusDeathRuling(combA, Some(SaveVersusDeathResult.Failed))

  protected val state = CombatState.empty.transitionWith(List(evtAddCombA, ApplyDamageEvent(combA, 41)))
  protected val rulingWithAnswer: Ruling[CombatState, _, _] = savedRuling
  protected val rulingWithoutAnswer: Ruling[CombatState, _, _] = SaveVersusDeathRuling(combA, None)
  protected val userPromptMessage: String = "Save versus death for Fighter [A]"

  def buildCases =
    "produce nothing on save" ! (savedRuling.generateCommands(state) must_== Nil) ^
      "produce healing on save 20" ! (savedAndHealRuling.generateCommands(state) must_== List(HealCommand(combA, 1))) ^
      "produce death thick on failed" ! (failedRuling.generateCommands(state) must_== List(FailDeathSaveCommand(combA))) ^
      end
}
