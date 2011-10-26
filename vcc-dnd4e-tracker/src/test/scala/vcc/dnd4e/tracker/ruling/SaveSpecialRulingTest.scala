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
import vcc.dnd4e.tracker.event.EventSourceSampleEvents
import vcc.tracker.Ruling
import vcc.dnd4e.tracker.transition.{UpdateEffectConditionCommand, CancelEffectCommand}
import vcc.dnd4e.tracker.common.{Effect, EffectID, CombatState}

class SaveSpecialRulingTest extends SpecificationWithJUnit with EventSourceSampleEvents {
  def is =
    "SaveSpecialRuling".title ^
      "have answer " ! e1 ^
      "produce on saved" ! e2 ^
      "produce on changed" ! e3 ^
      end

  private val eid = EffectID(combA, 1)
  private val rulings: List[Ruling[CombatState, _, _, _]] = List(
    SaveSpecialRuling(SaveSpecial.Against(eid, "bad -> worst"), Some(SaveSpecial.Saved)),
    SaveSpecialRuling(SaveSpecial.Against(eid, "bad -> worst"), Some(SaveSpecial.Changed("worst")))
  )
  private val state = CombatState.empty

  private def e1 = {
    rulings(0).hasDecision must beTrue
  }

  private def e2 = {
    rulings(0).generateCommands(state) must_== List(CancelEffectCommand(eid))
  }

  private def e3 = {
    rulings(1).generateCommands(state) must_== List(UpdateEffectConditionCommand(eid, Effect.Condition.Generic("worst", false)))
  }
}