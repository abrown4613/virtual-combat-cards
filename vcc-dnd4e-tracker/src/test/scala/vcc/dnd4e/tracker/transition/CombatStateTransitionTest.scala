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
package vcc.dnd4e.tracker.transition

import org.specs2.SpecificationWithJUnit
import vcc.dnd4e.tracker.common._
import vcc.controller.IllegalActionException
import vcc.dnd4e.tracker.event._

class CombatStateTransitionTest extends SpecificationWithJUnit with EventSourceSampleEvents with CombatStateEventSourceBehavior {

  def is =
    "Combat State Transactions" ^
      execAddCombatant ^
      defineCombatComment ^
      defineInitiativeBeforeCombatStart ^
      startCombat ^
      defineInitiativeAfterCombatStarted ^
      endCombat ^
      clearRoster ^
      combatantComments ^
      restCombatants ^
      end

  private def execAddCombatant = {
    val addA = AddCombatantTransition(Some(combA), null, entityPc1)
    "add combatant" ! (given(CombatState.empty) when addA then (AddCombatantEvent(addA.cid, null, entityPc1)))
  }

  private def defineCombatComment = {
    "Combat level comment" ^
      "Set comment to none if null" !
        (given(emptyState) when (SetCombatCommentTransition(null)) then (SetCombatCommentEvent(None))) ^
      "Set comment" !
        (given(emptyState) when (SetCombatCommentTransition("comment")) then (SetCombatCommentEvent(Some("comment")))) ^
      endp
  }

  //TODO Move this to some other file
  private def combatantComments = {
    val cmd = SetCombatantCommentTransition(combA, "message")
    val exception = new IllegalActionException("Cant set comment: Combatant " + combA + " does not exist")
    "Combatant Comment" ^
      "Set commant on existant combatant" !
        (given(emptyState, evtAddCombA) when (cmd) then (SetCombatantCommentEvent(combA, "message"))) ^
      "Setting comment combatant on inexistant combatant should fail" !
        (given(emptyState) when (cmd) failWith exception) ^
      endp
  }

  private def defineInitiativeBeforeCombatStart = {
    val evtInitA = AddCombatantToOrderEvent(InitiativeDefinition(combA, 5, List(10)))
    val evtInitAOld = AddCombatantToOrderEvent(InitiativeDefinition(combA, 5, List(3)))
    val cmd = SetInitiativeTransition(InitiativeDefinition(combA, 5, List(10)))
    val exception = new IllegalActionException("Combatant " + combA + " not in combat roster")
    "Defining initiative before combat start" ^
      "define initiative" ! (given(emptyState, evtAddCombA) when (cmd) then (evtInitA)) ^
      "redefine initiative" ! (given(emptyState, evtAddCombA, evtInitAOld) when (cmd) then (RemoveCombatantFromOrderEvent(combA), evtInitA)) ^
      "cant set initiative if combatant not in roster" ! (given(emptyState) when (cmd) failWith exception) ^
      endp
  }

  private def startCombat = {
    val evtInitA = AddCombatantToOrderEvent(InitiativeDefinition(combA, 5, List(10)))
    val exceptionNotInOrder = new IllegalActionException("Must have at least on combatant in order")
    val exceptionAlreadyStart = new IllegalActionException("Combat already started")

    "StartCombat" ^
      "cannot start if no combatant present" ! (given(emptyState) when (StartCombatTransition) failWith (exceptionNotInOrder)) ^
      "cannot start if no combatant in order" ! (given(emptyState, evtAddCombA, evtAddCombNoId) when (StartCombatTransition) failWith (exceptionNotInOrder)) ^
      "start must work" ! (given(emptyState, evtAddCombA, evtAddCombNoId, evtInitA) when (StartCombatTransition) then (StartCombatEvent)) ^
      "start on started is not allowed" ! (given(emptyState, evtAddCombA, evtAddCombNoId, evtInitA, StartCombatEvent) when (StartCombatTransition) failWith (exceptionAlreadyStart)) ^
      endp
  }

  private def defineInitiativeAfterCombatStarted = {
    val evtInitA = AddCombatantToOrderEvent(InitiativeDefinition(combA, 5, List(10)))
    val evtInit1 = AddCombatantToOrderEvent(InitiativeDefinition(comb1, 5, List(10)))
    val evtInitAOld = AddCombatantToOrderEvent(InitiativeDefinition(combA, 5, List(3)))
    val cmd = SetInitiativeTransition(InitiativeDefinition(combA, 5, List(10)))
    val exception = new IllegalActionException("Combatant " + combA + " is already in order")
    "Defining initiative after combat start" ^
      "define initiative" ! (given(emptyState, evtAddCombA, evtAddCombNoId, evtInit1, StartCombatEvent) when (cmd) then (evtInitA)) ^
      "redefine initiative (illegal)" ! (given(emptyState, evtAddCombA, evtInitAOld, StartCombatEvent) when (cmd) failWith (exception)) ^
      endp
  }

  def endCombat = {
    val notStartedException = new IllegalActionException("Combat not started")
    "EndCombat" ^
      "cant end combat that was not started" !
        (given(emptyState) when (EndCombatTransition) failWith (notStartedException)) ^
      "cant end combat that was not started, even if it looks full" !
        (given(emptyState, evtAddCombA, evtInitA) when (EndCombatTransition) failWith (notStartedException)) ^
      "end a properly started combat" !
        (given(emptyState, evtAddCombA, evtInitA, StartCombatEvent) when (EndCombatTransition) then (EndCombatEvent)) ^
      endp
  }

  def clearRoster = {
    val exception = new IllegalActionException("Can not clear while in combat")
    val rA: CombatStateEvent = RemoveCombatantFromRosterEvent(combA)
    val r1: CombatStateEvent = RemoveCombatantFromRosterEvent(comb1)
    val r2: CombatStateEvent = RemoveCombatantFromRosterEvent(comb2)
    "Clear combat" ^
      "clear monster on started combat must fail" !
        (given(emptyState, evtAddCombA, evtInitA, StartCombatEvent) when (ClearRosterTransition(true)) failWith exception) ^
      "clear all on started combat must fail" !
        (given(emptyState, evtAddCombA, evtInitA, StartCombatEvent) when (ClearRosterTransition(false)) failWith exception) ^
      "remove all the combat" !
        (given(emptyState, evtAddCombA, evtAddCombNoId, evtAddComb2) when (ClearRosterTransition(false)) then (contain(rA, r1, r2).only)) ^
      "remove only monsters" !
        (given(emptyState, evtAddCombA, evtAddCombNoId, evtAddComb2) when (ClearRosterTransition(true)) then (contain(r1, r2).only)) ^
      endp
  }

  private def restCombatants = {
    val exception = new IllegalActionException("Can not rest during combat")
    val shortRest = RestTransition(false)
    val extendedRest = RestTransition(true)
    val sRest1: CombatStateEvent = RestCombatantEvent(comb1, false)
    val sRestA: CombatStateEvent = RestCombatantEvent(combA, false)
    val eRest1: CombatStateEvent = RestCombatantEvent(comb1, true)
    val eRestA: CombatStateEvent = RestCombatantEvent(combA, true)
    "Resting combatant" ^
      "rest should fail if combat is started" !
        (given(emptyState, evtAddCombA, evtInitA, StartCombatEvent) when (shortRest) failWith exception) ^
      "extend should fail if combat is started" !
        (given(emptyState, evtAddCombA, evtInitA, StartCombatEvent) when (extendedRest) failWith exception) ^
      "extend should rest all" !
        (given(emptyState, evtAddCombA, evtAddCombNoId) when (extendedRest) then (contain(eRestA, eRest1))) ^
      "extend should rest all" !
        (given(emptyState, evtAddCombA, evtAddCombNoId) when (shortRest) then (contain(sRestA, sRest1))) ^
      endp
  }
}