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

class CombatStateTransitionTest extends SpecificationWithJUnit with SampleStateData with CombatStateEventSourceBehavior {

  // Library of events to build state
  private val emptyState = CombatState.empty
  private val evtAddA = AddCombatantEvent(Some(combA), null, entityPc1)
  private val evtAdd1 = AddCombatantEvent(None, null, entityMonster)

  private val addA = AddCombatantTransition(Some(combA), null, entityPc1)
  private val add1 = AddCombatantTransition(None, "first", entityMinion)
  private val add2 = AddCombatantTransition(None, null, entityMonster)
  private val rollA = SetInitiativeTransition(InitiativeDefinition(combA, 5, List(10)))
  private val roll1 = SetInitiativeTransition(InitiativeDefinition(comb1, 4, List(9)))
  private val roll2 = SetInitiativeTransition(InitiativeDefinition(comb2, 4, List(19)))

  def is =
    "AddCombatantTransition" ^
      "add combatant" ! e1 ^
      endp ^
      "ApplyRest should" ^
      "  fail if in combat" ! restCase().testStillInCombat ^
      "  update combatants otherwise" ! restCase().restExecuted ^
      endp ^
      "SetCombatComment should" ^
      "  set to some if we put a message" ! e4 ^
      "  set to none if null used" ! e5 ^
      endp ^
      defineInitiativeBeforeCombatStart ^
      startCombat ^
      defineInitiativeAfterCombatStarted ^
      "EndCombat and Clear" ^
      "  should only End started combat " ! testEndNotCombatStarted ^
      "  should end combat" ! testEndCombat ^
      "  should only clear with combat ended" ! testClearWhileInCombatant ^
      "  should clear monster" ! testClearMonster ^
      "  should clear everything" ! testClearAll ^
      endp ^
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

  private def e1() = {
    given(CombatState.empty) when addA then (AddCombatantEvent(addA.cid, null, entityPc1))
  }

  private def e4 = {
    val trans = SetCombatCommentTransition("new comment")
    val state = CombatState.empty

    (state.comment must_== None) and
      (trans.transition(state).comment must_== Some("new comment"))
  }

  private def e5 = {
    val trans = SetCombatCommentTransition("new comment")
    val trans2 = SetCombatCommentTransition(null)
    val state = CombatState.empty

    (trans2.transition(trans.transition(state)).comment must_== None)
  }

  private def testSetFirstInitiative = {
    val state = transform(CombatState.empty, add1, add2, addA, rollA)
    (state.order.sequence must_== List(ioA0)) and (state.isCombatStarted must beFalse)
  }

  private def testSetTwoInitiative = {
    val state = transform(CombatState.empty, add1, add2, addA, rollA, roll1)
    (state.order.sequence must_== List(ioA0, io1_0)) and (state.isCombatStarted must beFalse)
  }

  private def testSetTwoInitiativeAndStart = {
    val state = transform(CombatState.empty, add1, add2, addA, rollA, roll1, StartCombatTransition)
    (state.order.sequence must_== List(ioA0, io1_0)) and (state.isCombatStarted must beTrue) and
      (state.order.nextUp must_== Some(ioA0))
  }

  private def testSetTwoInitiativeAndStartThenReRoll = {
    transform(CombatState.empty, add1, add2, addA, rollA, roll1, StartCombatTransition, roll1) must
      throwAn(new IllegalActionException("Combatant " + comb1 + " is already in order"))
  }

  private def testSetTwoInitiativeAndStartThenRollThird = {
    val state = transform(CombatState.empty, add1, add2, addA, rollA, roll1, StartCombatTransition, roll2)
    (state.order.sequence must_== List(io2_0, ioA0, io1_0)) and (state.isCombatStarted must beTrue) and
      (state.order.nextUp must_== Some(ioA0))
  }


  private def testStartCombatWithNotInitiative = {
    transform(CombatState.empty, add1, StartCombatTransition) must
      throwAn(new IllegalActionException("Must have at least on combatant in order"))
  }

  private def testSetInitiativeToNonPresentCombatant = {
    transform(CombatState.empty, add1, rollA) must
      throwAn(new IllegalActionException("Combatant " + combA + " not in combat roster"))
  }

  private def testStartStart = {
    transform(CombatState.empty, addA, rollA, StartCombatTransition, StartCombatTransition) must
      throwAn(new IllegalActionException("Combat already started"))
  }

  private def testReRollBeforeStart = {
    val r = SetInitiativeTransition(InitiativeDefinition(combA, 0, List(4, 19)))
    val state = transform(CombatState.empty, add1, add2, addA, rollA, roll1, r)
    (state.order.sequence must_== List(ioA1, io1_0, ioA0))
  }

  private def testEndNotCombatStarted = {
    transform(CombatState.empty, addA, rollA, EndCombatTransition) must
      throwAn(new IllegalActionException("Combat not started"))
  }

  private def testEndCombat = {
    transform(CombatState.empty, addA, rollA, StartCombatTransition, EndCombatTransition).isCombatStarted must beFalse
  }

  private def testClearMonster = {
    val state = transform(CombatState.empty, addA, rollA, add1, add2, ClearRosterTransition(true))

    (state.roster.isDefinedAt(combA) must beTrue) and
      (state.roster.isDefinedAt(comb1) must beFalse) and
      (state.roster.isDefinedAt(comb2) must beFalse)
  }

  private def testClearAll = {
    transform(CombatState.empty, addA, rollA, add1, add2, ClearRosterTransition(false)) must_== CombatState.empty
  }

  private def testClearWhileInCombatant = {
    transform(CombatState.empty, addA, rollA, StartCombatTransition, ClearRosterTransition(false)) must
      throwAn(new IllegalActionException("Can not clear while in combat"))
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
      "define initiative" ! (given(emptyState, evtAddA) when (cmd) then (evtInitA)) ^
      "redefine initiative" ! (given(emptyState, evtAddA, evtInitAOld) when (cmd) then (RemoveCombatantFromOrderEvent(combA), evtInitA)) ^
      "cant set initiative if combatant not in roster" ! (given(emptyState) when (cmd) failWith exception) ^
      endp
  }

  private def startCombat = {
    val evtInitA = AddCombatantToOrderEvent(InitiativeDefinition(combA, 5, List(10)))
    val exceptionNotInOrder = new IllegalActionException("Must have at least on combatant in order")
    val exceptionAlreadyStart = new IllegalActionException("Combat already started")

    "StartCombat" ^
      "cannot start if no combatant present" ! (given(emptyState) when (StartCombatTransition) failWith (exceptionNotInOrder)) ^
      "cannot start if no combatant in order" ! (given(emptyState, evtAddA, evtAdd1) when (StartCombatTransition) failWith (exceptionNotInOrder)) ^
      "start must work" ! (given(emptyState, evtAddA, evtAdd1, evtInitA) when (StartCombatTransition) then (StartCombatEvent)) ^
      "start on started is not allowed" ! (given(emptyState, evtAddA, evtAdd1, evtInitA, StartCombatEvent) when (StartCombatTransition) failWith (exceptionAlreadyStart)) ^
      endp
  }

  private def defineInitiativeAfterCombatStarted = {
    val evtInitA = AddCombatantToOrderEvent(InitiativeDefinition(combA, 5, List(10)))
    val evtInit1 = AddCombatantToOrderEvent(InitiativeDefinition(comb1, 5, List(10)))
    val evtInitAOld = AddCombatantToOrderEvent(InitiativeDefinition(combA, 5, List(3)))
    val cmd = SetInitiativeTransition(InitiativeDefinition(combA, 5, List(10)))
    val exception = new IllegalActionException("Combatant " + combA + " is already in order")
    "Defining initiative after combat start" ^
      "define initiative" ! (given(emptyState, evtAddA, evtAdd1, evtInit1, StartCombatEvent) when (cmd) then (evtInitA)) ^
      "redefine initiative (illegal)" ! (given(emptyState, evtAddA, evtInitAOld, StartCombatEvent) when (cmd) failWith (exception)) ^
      endp
  }
}