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
import vcc.dnd4e.tracker.common._

class RosterAndOrderEventTest extends SpecificationWithJUnit with EventSourceSampleEvents with Mockito {

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
      "RemoveCombatantFromOrderEvent" ! execRemoveCombatantFromRosterEvent ^
      "RemoveCombatantFromOrderEvent when some have initiative" ! execRemoveCombatantFromRosterEventWithInitiative ^
      "SetCombatComment " ! execCombatComment ^
      "SetCombatCommentClear " ! execCombatCommentClear ^
      end

  private def e1 = {
    val cs = evtAddCombA.transition(CombatState.empty)
    (cs.isCombatStarted must beFalse) and
      (cs.roster.entries must haveKey(combA))
  }

  private def e2 = {
    val cs = evtAddCombNoId.transition(CombatState.empty)
    (cs.isCombatStarted must beFalse) and
      (cs.roster.entries must haveKey(comb1))
  }

  private def e3 = {
    val cs = CombatState.empty.transitionWith(List(evtAddCombA, evtAddComb2, evtAddCombNoId))
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

  private def execRemoveCombatantFromRosterEvent = {
    val state = CombatState.empty.transitionWith(List(evtAddCombA, evtAddCombNoId))
    val newState = RemoveCombatantFromRosterEvent(combA).transition(state)

    (newState.roster.isDefinedAt(combA) must beFalse) and
      (state.roster.isDefinedAt(combA) must beTrue)
  }

  private def execRemoveCombatantFromRosterEventWithInitiative = {
    val state = CombatState.empty.transitionWith(List(evtAddCombA, evtAddCombNoId, evtAddComb2, evtInitA))
    val newState = RemoveCombatantFromRosterEvent(combA).transition(state)

    (newState.roster.isDefinedAt(combA) must beFalse) and
      (newState.order.isInSequence(combA) must beFalse) and
      (state.order.isInSequence(combA) must beTrue) and
      (state.roster.entries.keys must contain(comb1, comb2))
  }

  private def execCombatComment = {
    val state = SetCombatCommentEvent(Some("new comment")).transition(emptyState)
    (state.comment must_== Some("new comment"))
  }

  private def execCombatCommentClear = {
    val state = SetCombatCommentEvent(None).transition(emptyState)
    (state.comment must_== None)
  }
}

