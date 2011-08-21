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
import org.specs2.mock.Mockito
import vcc.controller.IllegalActionException
import vcc.dnd4e.tracker.event._

class CombatStateTransitionTest extends SpecificationWithJUnit with EventSourceSampleEvents with CombatStateEventSourceBehavior {

  private val addA = AddCombatantTransition(Some(combA), null, entityPc1)

  def is =
    "AddCombatantTransition" ^
      "add combatant" ! execAddCombatant ^
      "ApplyRest should" ^
      "  fail if in combat" ! restCase().testStillInCombat ^
      "  update combatants otherwise" ! restCase().restExecuted ^
      endp ^
      defineCombatComment ^
      defineInitiativeBeforeCombatStart ^
      startCombat ^
      defineInitiativeAfterCombatStarted ^
      endCombat ^
      clearRoster ^
      "Set Combatant comment" ^
      "  should fail if combatant not defined" ! testMissingCombatantComment ^
      "  should change combatant comment" ! testCombatantComment ^
      end

  case class restCase() extends Mockito {
    val mELA = mock[EffectList]
    val mEL1 = mock[EffectList]
    val mHTA = mock[HealthTracker]
    val mHT1 = mock[HealthTracker]
    val state = StateBuilder.emptyState().
      addCombatant(Some(combA), null, entityPc1).
      addCombatant(None, null, entityMonster).
      setInitiative(combA, 12).
      setInitiative(comb1, 10).
      modifyEffectList(combA, x => mELA).
      modifyEffectList(comb1, x => mEL1).
      modifyHealth(combA, x => mHTA).
      modifyHealth(comb1, x => mHT1).
      done

    private val trans = RestTransition(true)

    def testStillInCombat = {
      trans.transition(state.startCombat()) must throwAn(new IllegalActionException("Can not rest during combat"))
    }

    def restExecuted = {
      trans.transition(state)
      (there was one(mELA).transformAndFilter(EffectTransformation.applyRest)) and
        (there was one(mEL1).transformAndFilter(EffectTransformation.applyRest)) and
        (there was one(mHTA).rest(true)) and
        (there was one(mHT1).rest(true))
    }
  }

  private def transform(state: CombatState, trans: CombatTransition*): CombatState = {
    trans.foldLeft(state)((s, x) => x.transition(s))
  }

  private def execAddCombatant = {
    given(CombatState.empty) when addA then (AddCombatantEvent(addA.cid, null, entityPc1))
  }

  private def defineCombatComment = {
    "Combat level comment" ^
      "Set comment to none if null" !
        (given(emptyState) when (SetCombatCommentTransition(null)) then (SetCombatCommentEvent(None))) ^
      "Set comment" !
        (given(emptyState) when (SetCombatCommentTransition("comment")) then (SetCombatCommentEvent(Some("comment")))) ^
      endp
  }

  private def testCombatantComment = {
    val trans = SetCombatantCommentTransition(combA, "new comment")
    val state = transform(CombatState.empty, addA)
    val nState = trans.transition(state)

    state.roster.combatantDiff(nState.roster) must_== Set(CombatantCommentDiff(combA, "", "new comment"))
  }

  private def testMissingCombatantComment = {
    val trans = SetCombatantCommentTransition(combA, "new comment")

    trans.transition(CombatState.empty) must throwA[NoSuchElementException]
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
}