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
import vcc.dnd4e.tracker.transition.{HealCommand, FailDeathSaveCommand}
import vcc.dnd4e.tracker.event.{ApplyDamageEvent, EventSourceSampleEvents}
import vcc.dnd4e.tracker.common.{CombatState}

class SaveVersusDeathRulingTest extends SpecificationWithJUnit with EventSourceSampleEvents {
  def is =
    "SaveVersusDeath".title ^
      "have proper is user message" ! e0 ^
      "have answer " ! e1 ^
      "produce nothing on save" ! e2 ^
      "produce produce healing on save 20" ! e3 ^
      "produce produce death thick on failed" ! e4 ^
      end

  private val savedRuling = SaveVersusDeathRuling(combA, Some(SaveVersusDeathResult.Saved))
  private val savedAndHealRuling = SaveVersusDeathRuling(combA, Some(SaveVersusDeathResult.SaveAndHeal))
  private val failedRuling = SaveVersusDeathRuling(combA, Some(SaveVersusDeathResult.Failed))

  private val state = CombatState.empty.transitionWith(List(evtAddCombA, ApplyDamageEvent(combA, 41)))

  private def e0 = {
    savedRuling.userPrompt(state) must_== "Save versus death for Fighter [A]"
  }

  private def e1 = {
    savedRuling.hasDecision must beTrue
  }

  private def e2 = {
    savedRuling.generateCommands(state) must_== Nil
  }

  private def e3 = {
    savedAndHealRuling.generateCommands(state) must_== List(HealCommand(combA, 1))
  }

  private def e4 = {
    failedRuling.generateCommands(state) must_== List(FailDeathSaveCommand(combA))
  }
}
