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

import org.specs2.mock.Mockito
import org.specs2.{SpecificationWithJUnit}
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.StateLensFactory
import vcc.scalaz.Lens
import java.lang.Exception
import vcc.controller.IllegalActionException

class InitiativeTransitionStepTest extends SpecificationWithJUnit with SampleStateData {

  def is =
    "DelayEffectListTransformStep" ^
      "characters delays" ! effectTransformations().characterDelays ^
      "monster delays" ! effectTransformations().monsterDelays ^
      endp ^
      "EffectListTransformStep" ^
      "  apply effect transformation on all combatant" ! effectTransformations().applyToAllEffect ^
      endp ^
      "simple transactions" ^
      "  MoveBeforeOtherStep should move before other" ! m().moveBeforeOther ^
      "  MoveBeforeFirstStep should move someone before first" ! m().moveBeforeFirst ^
      "  RotateRobinStep should rotate robin" ! m().rotateRobin ^
      "  SetRobinStep should set robin to entry specified" ! m().robinHeadUpdate ^
      endp ^
      "InitiativeTrackerUpdateStep" ^
      "  throw exception if action is not allowed" ! its().illegalTransition ^
      "  update tracker if action is allowed" ! its().allowedTransition ^
      endp


  case class effectTransformations() extends Mockito {

    def setupMock() = {
      val m = mock[EffectList]
      m.transformAndFilter(any) returns m
      m
    }

    val (mELA, mELB, mEL1, mEL2) = (setupMock(), setupMock(), setupMock(), setupMock())

    val state: CombatState = StateBuilder.emptyState().
      addCombatant(Some(combA), null, entityPc1).
      addCombatant(Some(combB), null, entityPc2).
      addCombatant(None, null, entityMinion).
      addCombatant(None, null, entityMonster).
      modifyEffectList(combA, x => mELA).
      modifyEffectList(combB, x => mELB).
      modifyEffectList(comb1, x => mEL1).
      modifyEffectList(comb2, x => mEL2).
      done

    def monsterDelays = {
      val ioi = io1_0
      val trans = DelayEffectListTransformStep(ioi)
      state.transitionWith(List(trans))
      (there was one(mELA).transformAndFilter(EffectTransformation.processDelay(false, ioi))) and
        (there was one(mELB).transformAndFilter(EffectTransformation.processDelay(false, ioi))) and
        (there was one(mEL1).transformAndFilter(EffectTransformation.processDelay(true, ioi))) and
        (there was one(mEL2).transformAndFilter(EffectTransformation.processDelay(true, ioi)))
    }

    def characterDelays = {
      val ioi = ioA0
      val trans = DelayEffectListTransformStep(ioi)
      state.transitionWith(List(trans))
      (there was one(mELA).transformAndFilter(EffectTransformation.processDelay(true, ioi))) and
        (there was one(mELB).transformAndFilter(EffectTransformation.processDelay(true, ioi))) and
        (there was one(mEL1).transformAndFilter(EffectTransformation.processDelay(false, ioi))) and
        (there was one(mEL2).transformAndFilter(EffectTransformation.processDelay(false, ioi)))
    }

    def applyToAllEffect = {
      val mET = mock[EffectTransformation]

      val trans = EffectListTransformStep(mET)
      state.transitionWith(List(trans))
      (there was one(mELA).transformAndFilter(mET)) and
        (there was one(mELB).transformAndFilter(mET)) and
        (there was one(mEL1).transformAndFilter(mET)) and
        (there was one(mEL2).transformAndFilter(mET))
    }
  }

  case class m() extends Mockito {
    val mState = mock[CombatState]
    val mState2 = mock[CombatState]
    val mLF = mock[StateLensFactory]
    val mOrder = mock[InitiativeOrder]
    mLF.orderLens returns Lens(x => mOrder, (s, o) => mState2)

    def moveBeforeOther = {
      val trans = MoveBeforeOtherStep(ioA0, ioB0)
      val ns = trans.transition(mLF, mState)
      (there was one(mOrder).moveBefore(ioA0, ioB0)) and (ns must_== mState2)
    }

    def moveBeforeFirst = {
      val trans = MoveBeforeFirstStep(ioA0)
      mOrder.nextUp returns Some(io1_0)
      val ns = trans.transition(mLF, mState)
      (there was one(mOrder).moveBefore(ioA0, io1_0)) and (ns must_== mState2)
    }

    def rotateRobin = {
      val trans = RotateRobinStep
      val ns = trans.transition(mLF, mState)
      (there was one(mOrder).rotate()) and (ns must_== mState2)
    }

    def robinHeadUpdate = {
      val trans = SetRobinStep(ioA0)
      val ns = trans.transition(mLF, mState)
      (there was one(mOrder).setNextUp(ioA0)) and (ns must_== mState2)
    }
  }

  case class its() extends Mockito {
    val mOrder = mock[InitiativeOrder]
    val mLF = mock[StateLensFactory]
    val mRules = mock[CombatState.Rules]
    val mState = mock[CombatState]
    val mState2 = mock[CombatState]
    val mITA = mock[InitiativeTracker]
    val mITF = mock[InitiativeTracker]
    val mAction = mock[InitiativeAction.Value]
    mLF.orderLens returns Lens(s => mOrder, (s, o) => throw new Exception("Not allowed"))
    mLF.initiativeTrackerLens(ioA0) returns Lens(s => mITA, (s, o) => mState2)
    mState.rules returns mRules
    mOrder.nextUp returns Some(io1_0)
    mOrder.tracker returns Map(io1_0 -> mITF, ioA0 -> mITA)

    /**
     * In this case InitiativeTracker cant be execute the actions, so an IllegalActionException must be thrown
     */
    def illegalTransition = {
      mRules.canInitiativeOrderPerform(mState, ioA0, mAction) returns false
      val t = InitiativeTrackerUpdateStep(ioA0, mAction)
      (t.transition(mLF, mState) must throwA[IllegalActionException]) and
        (there was one(mRules).canInitiativeOrderPerform(mState, ioA0, mAction))
    }

    /**
     * Action is allowed so call the initiative track to update it.
     */
    def allowedTransition = {
      mRules.canInitiativeOrderPerform(mState, ioA0, mAction) returns true
      val t = InitiativeTrackerUpdateStep(ioA0, mAction)
      val ns = t.transition(mLF, mState)
      (there was one(mRules).canInitiativeOrderPerform(mState, ioA0, mAction) then
        one(mITA).transform(mITF, mAction)) and (ns must_== mState2)
    }
  }

}
