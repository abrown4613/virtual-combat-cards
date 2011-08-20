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
package vcc.dnd4e.tracker.event

import org.specs2.SpecificationWithJUnit
import org.specs2.mock.Mockito
import vcc.dnd4e.tracker.common.{InitiativeDefinition, InitiativeOrder, CombatState, SampleStateData}

class AddCombatantEventTest extends SpecificationWithJUnit with SampleStateData with Mockito {

  def is =
    "AddCombatantEvent" ^
      "add a combatant with a defined combatant ID" ! e1 ^
      "add a combatant without a defined combatnat ID" ! e2 ^
      "chained addition" ! e3 ^
      endp ^
      "Start combat execution" ! execStartCombatEvent ^
      "End combat execution" ! execEndCombatEvent ^
      "RemoveCombatantFromOrderEvent" ! execRemoveCombatantFromOrderEvent ^
      "AddCombatantToOrderEvent" ! execAddCombatantToOrderEvent ^
      end

  private val evtAddA = AddCombatantEvent(Some(combA), null, entityPc1)
  private val evtAdd1 = AddCombatantEvent(None, null, entityMinion)
  private val evtAdd2 = AddCombatantEvent(Some(comb2), null, entityMonster)

  private def e1 = {
    val cs = evtAddA.transition(CombatState.empty)
    (cs.isCombatStarted must beFalse) and
      (cs.roster.entries must haveKey(combA))
  }

  private def e2 = {
    val cs = evtAdd1.transition(CombatState.empty)
    (cs.isCombatStarted must beFalse) and
      (cs.roster.entries must haveKey(comb1))
  }

  private def e3 = {
    val cs = CombatState.empty.transitionWith(List(evtAddA, evtAdd2, evtAdd1))
    (cs.isCombatStarted must beFalse) and
      (cs.roster.entries.keySet must_== Set(combA, comb1, comb2))
  }

  private def execEndCombatEvent = {
    val state = mock[CombatState]
    EndCombatEvent.transition(state)
    there was one(state).endCombat()
  }

  private def execStartCombatEvent = {
    val state = mock[CombatState]
    StartCombatEvent.transition(state)
    there was one(state).startCombat()
  }

  private def stateWithMockOrder = {
    val state = CombatState.empty
    state.lensFactory.orderLens.mod(state, r => mock[InitiativeOrder])
  }

  private def execRemoveCombatantFromOrderEvent = {
    val state = stateWithMockOrder
    RemoveCombatantFromOrderEvent(combA).transition(state)
    there was one(state.order).removeCombatant(combA)
  }

  private def execAddCombatantToOrderEvent = {
    val state = stateWithMockOrder
    val iDef = InitiativeDefinition(combA, 5, List(5))
    AddCombatantToOrderEvent(iDef).transition(state)
    there was one(state.order).setInitiative(iDef)
  }
}

