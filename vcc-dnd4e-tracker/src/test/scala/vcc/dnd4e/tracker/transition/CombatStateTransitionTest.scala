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
//$Id$
package vcc.dnd4e.tracker.transition

import org.specs2.SpecificationWithJUnit
import vcc.dnd4e.tracker.StateLensFactory
import vcc.dnd4e.tracker.common._
import org.specs2.mock.Mockito
import vcc.controller.IllegalActionException

class CombatStateTransitionTest extends SpecificationWithJUnit with SampleStateData {

  val addA = AddCombatantTransition(Some(combA), null, entityPc1)
  val add1 = AddCombatantTransition(None, "first", entityMinion)
  val add2 = AddCombatantTransition(None, null, entityMonster)
  val rollA = SetInitiativeTransition(InitiativeDefinition(combA, 5, List(10)))
  val roll1 = SetInitiativeTransition(InitiativeDefinition(comb1, 4, List(9)))
  val roll2 = SetInitiativeTransition(InitiativeDefinition(comb2, 4, List(19)))

  def is =
    "AddCombatantTransition" ^
      "add one combatant" ! e1() ^
      "add two combatant" ! e2() ^
      "add three combatant" ! e3() ^
      endp ^
      "ApplyRest should" ^
      "  fail if in combat" ! restCase().testStillInCombat ^
      "  update combatants otherwise" ! restCase().restExecuted ^
      endp ^
      "SetCombatComment should" ^
      "  set to some if we put a message" ! e4 ^
      "  set to none if null used" ! e5 ^
      endp ^
      "Initiative and RoundStarting" ^
      "  start combat with not initiative" ! testStartCombatWithNotInitiative ^
      "  set initiative of missing combatant" ! testSetInitiativeToNonPresentCombatant ^
      "  set one initiative" ! testSetFirstInitiative ^
      "  set two initiative" ! testSetTwoInitiative ^
      "  set two initiative and start combat" ! testSetTwoInitiativeAndStart ^
      "  set two initiative and start combat and reroll" ! testSetTwoInitiativeAndStartThenReRoll ^
      "  set two initiative and start combat add third" ! testSetTwoInitiativeAndStartThenRollThird ^
      "  start and start is illegal" ! testStartStart ^
      "  set two initiative then reroll one" ! testReRollBeforeStart ^
      endp ^
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

    val trans = RestTransition(true)

    def testStillInCombat = {
      trans.transition(StateLensFactory, state.startCombat()) must throwAn(new IllegalActionException("Can not rest during combat"))
    }

    def restExecuted = {
      trans.transition(StateLensFactory, state)
      (there was one(mELA).transformAndFilter(EffectTransformation.applyRest)) and
        (there was one(mEL1).transformAndFilter(EffectTransformation.applyRest)) and
        (there was one(mHTA).rest(true)) and
        (there was one(mHT1).rest(true))
    }
  }

  def transform(state: CombatState, trans: CombatTransition*): CombatState = {
    trans.foldLeft(state)((s, x) => x.transition(StateLensFactory, s))
  }

  def e1() = {
    val cs = transform(CombatState.empty, addA)
    (cs.isCombatStarted must beFalse) and
      (cs.roster.entries must haveKey(combA))
  }

  def e2() = {
    val cs = transform(CombatState.empty, addA, add1)
    (cs.isCombatStarted must beFalse) and
      (cs.roster.entries.keySet must_== Set(combA, comb1))
  }

  def e3() = {
    val cs = transform(CombatState.empty, addA, add1, add2)
    (cs.isCombatStarted must beFalse) and
      (cs.roster.entries.keySet must_== Set(combA, comb1, comb2))
  }

  def e4 = {
    val trans = SetCombatCommentTransition("new comment")
    val state = CombatState.empty

    (state.comment must_== None) and
      (trans.transition(StateLensFactory, state).comment must_== Some("new comment"))
  }

  def e5 = {
    val trans = SetCombatCommentTransition("new comment")
    val trans2 = SetCombatCommentTransition(null)
    val state = CombatState.empty

    (trans2.transition(StateLensFactory, trans.transition(StateLensFactory, state)).comment must_== None)
  }

  def testSetFirstInitiative = {
    val state = transform(CombatState.empty, add1, add2, addA, rollA)
    (state.order.order must_== List(ioA0)) and (state.isCombatStarted must beFalse)
  }

  def testSetTwoInitiative = {
    val state = transform(CombatState.empty, add1, add2, addA, rollA, roll1)
    (state.order.order must_== List(ioA0, io1_0)) and (state.isCombatStarted must beFalse)
  }

  def testSetTwoInitiativeAndStart = {
    val state = transform(CombatState.empty, add1, add2, addA, rollA, roll1, StartCombatTransition)
    (state.order.order must_== List(ioA0, io1_0)) and (state.isCombatStarted must beTrue) and
      (state.order.nextUp must_== Some(ioA0))
  }

  def testSetTwoInitiativeAndStartThenReRoll = {
    transform(CombatState.empty, add1, add2, addA, rollA, roll1, StartCombatTransition, roll1) must
      throwAn(new IllegalActionException("Combatant " + comb1 + " is already in order"))
  }

  def testSetTwoInitiativeAndStartThenRollThird = {
    val state = transform(CombatState.empty, add1, add2, addA, rollA, roll1, StartCombatTransition, roll2)
    (state.order.order must_== List(io2_0, ioA0, io1_0)) and (state.isCombatStarted must beTrue) and
      (state.order.nextUp must_== Some(ioA0))
  }


  def testStartCombatWithNotInitiative = {
    transform(CombatState.empty, add1, StartCombatTransition) must
      throwAn(new IllegalActionException("Must have at least on combatant in order"))
  }

  def testSetInitiativeToNonPresentCombatant = {
    transform(CombatState.empty, add1, rollA) must
      throwAn(new IllegalActionException("Combatant " + combA + " not in combat roster"))
  }

  def testStartStart = {
    transform(CombatState.empty, addA, rollA, StartCombatTransition, StartCombatTransition) must
      throwAn(new IllegalActionException("Combat already started"))
  }

  def testReRollBeforeStart = {
    val r = SetInitiativeTransition(InitiativeDefinition(combA, 0, List(4, 19)))
    val state = transform(CombatState.empty, add1, add2, addA, rollA, roll1, r)
    (state.order.order must_== List(ioA1, io1_0, ioA0))
  }

  def testEndNotCombatStarted = {
    transform(CombatState.empty, addA, rollA, EndCombatTransition) must
      throwAn(new IllegalActionException("Combat not started"))
  }

  def testEndCombat = {
    transform(CombatState.empty, addA, rollA, StartCombatTransition, EndCombatTransition).isCombatStarted must beFalse
  }

  def testClearMonster = {
    val state = transform(CombatState.empty, addA, rollA, add1, add2, ClearRosterTransition(true))

    (state.roster.isDefinedAt(combA) must beTrue) and
      (state.roster.isDefinedAt(comb1) must beFalse) and
      (state.roster.isDefinedAt(comb2) must beFalse)
  }

  def testClearAll = {
    transform(CombatState.empty, addA, rollA, add1, add2, ClearRosterTransition(false)) must_== CombatState.empty
  }

  def testClearWhileInCombatant = {
    transform(CombatState.empty, addA, rollA, StartCombatTransition, ClearRosterTransition(false)) must
      throwAn(new IllegalActionException("Can not clear while in combat"))
  }

  def testCombatantComment = {
    val trans = SetCombatantCommentTransition(combA, "new comment")
    val state = transform(CombatState.empty, addA)
    val nState = trans.transition(StateLensFactory, state)

    state.roster.combatantDiff(nState.roster) must_== Set(CombatantCommentDiff(combA, "", "new comment"))
  }

  def testMissingCombatantComment = {
    val trans = SetCombatantCommentTransition(combA, "new comment")

    trans.transition(StateLensFactory, CombatState.empty) must throwA[NoSuchElementException]
  }
}