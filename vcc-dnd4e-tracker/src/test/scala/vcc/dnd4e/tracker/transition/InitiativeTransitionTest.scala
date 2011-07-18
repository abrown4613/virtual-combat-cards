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
import org.specs2.mock.Mockito
import vcc.dnd4e.tracker.StateLensFactory
import vcc.dnd4e.tracker.common._
import vcc.scalaz.Lens
import vcc.controller.IllegalActionException

class InitiativeTransitionTest extends SpecificationWithJUnit {
  def is =
    "on EndRound" ^
      "update a normal Acting combatant" ! endRound().doUpdateEffects() ^
      "update a Delaying combatant but dont rotate" ! endRound().doUpdateAfterDelaying() ^
      endp ^
      "on StartRound" ^
      "  update tracker and effect lists" ! startRound().doUpdateAndEffects() ^
      endp ^
      "on DelayRound" ^
      "  update tracker and call proper effect transformation" ! delayRound().doUpdateAndEffects() ^
      endp ^
      "on ReadyAction" ^
      "  update tracker and rotate" ! readyRound().doIt() ^
      endp ^
      "on ExecuteReady" ^
      "  update tracker and move up" ! executeReady().doIt() ^
      endp ^
      "on MoveUp from a delay" ^
      "  update tracker and move up and rotate to delaying" ! moveUpRound().doIt() ^
      endp ^
      "on MoveBefore" ^
      "  throw exceptions if move is not allowed" ! moveBefore().illegalMove() ^
      "  adjust when first moves" ! moveBefore().validMoveOfFirst() ^
      "  adjust when other than first moves" ! moveBefore().validMoveOfNotFirst() ^
      endp

  trait TestBase extends Mockito {
    val combA = CombatantID("A")
    val combB = CombatantID("B")
    val ioA0 = InitiativeOrderID(combA, 0)
    val ioB0 = InitiativeOrderID(combB, 0)
    val mState = mock[CombatState]
    val mState2 = mock[CombatState]
    val mLF = mock[StateLensFactory]

    val transition: CombatTransition
  }

  case class endRound() extends TestBase {

    val transition = EndRoundTransition(ioA0)

    def doUpdateEffects() = {
      val lens = Lens[CombatState, InitiativeTracker]((x) => InitiativeTracker(ioA0, 2, 0, InitiativeState.Acting), (s, v) => s)
      mLF.initiativeTrackerLens(ioA0) returns lens

      mState.transitionWith(List(
        InitiativeTrackerUpdateStep(ioA0, InitiativeAction.EndRound),
        RotateRobinStep,
        EffectListTransformStep(EffectTransformation.endRound(ioA0)))) returns mState2

      (transition.transition(mLF, mState) must_== mState2) and
        (there was one(mState).transitionWith(List(
          InitiativeTrackerUpdateStep(ioA0, InitiativeAction.EndRound),
          RotateRobinStep,
          EffectListTransformStep(EffectTransformation.endRound(ioA0)))))
    }

    def doUpdateAfterDelaying() = {
      val lens = Lens[CombatState, InitiativeTracker]((x) => InitiativeTracker(ioA0, 2, 0, InitiativeState.Delaying), (s, v) => s)
      mLF.initiativeTrackerLens(ioA0) returns lens

      mState.transitionWith(List(
        InitiativeTrackerUpdateStep(ioA0, InitiativeAction.EndRound),
        EffectListTransformStep(EffectTransformation.endRound(ioA0)))) returns mState2

      (transition.transition(mLF, mState) must_== mState2) and
        (there was one(mState).transitionWith(List(
          InitiativeTrackerUpdateStep(ioA0, InitiativeAction.EndRound),
          EffectListTransformStep(EffectTransformation.endRound(ioA0)))))
    }
  }

  case class startRound() extends TestBase {
    val transition = StartRoundTransition(ioA0)

    def doUpdateAndEffects() = {
      mState.transitionWith(List(
        InitiativeTrackerUpdateStep(ioA0, InitiativeAction.StartRound),
        EffectListTransformStep(EffectTransformation.startRound(ioA0)))) returns mState2

      val nState = transition.transition(mLF, mState)
      (there was one(mState).transitionWith(List(
        InitiativeTrackerUpdateStep(ioA0, InitiativeAction.StartRound),
        EffectListTransformStep(EffectTransformation.startRound(ioA0))))) and
        (nState must_== mState2)
    }
  }

  case class delayRound() extends TestBase {
    val transition = DelayTransition(ioA0)

    def doUpdateAndEffects() = {
      mState.transitionWith(List(
        InitiativeTrackerUpdateStep(ioA0, InitiativeAction.DelayAction),
        RotateRobinStep,
        DelayEffectListTransformStep(ioA0))) returns mState2

      val nState = transition.transition(mLF, mState)
      (there was one(mState).transitionWith(List(
        InitiativeTrackerUpdateStep(ioA0, InitiativeAction.DelayAction),
        RotateRobinStep,
        DelayEffectListTransformStep(ioA0)))) and
        (nState must_== mState2)
    }
  }

  case class readyRound() extends TestBase {
    val transition = ReadyActionTransition(ioA0)

    def doIt() = {
      mState.transitionWith(List(InitiativeTrackerUpdateStep(ioA0, InitiativeAction.ReadyAction))) returns mState2

      val nState = transition.transition(mLF, mState)
      (there was one(mState).transitionWith(List(InitiativeTrackerUpdateStep(ioA0, InitiativeAction.ReadyAction)))) and
        (nState must_== mState2)

    }
  }

  case class executeReady() extends TestBase {
    val transition = ExecuteReadyTransition(ioA0)

    def doIt() = {
      mState.transitionWith(List(
        InitiativeTrackerUpdateStep(ioA0, InitiativeAction.ExecuteReady),
        MoveBeforeFirstStep(ioA0))) returns mState2

      val nState = transition.transition(mLF, mState)
      (there was one(mState).transitionWith(List(
        InitiativeTrackerUpdateStep(ioA0, InitiativeAction.ExecuteReady),
        MoveBeforeFirstStep(ioA0)))) and
        (nState must_== mState2)
    }
  }

  case class moveUpRound() extends TestBase {
    val transition = MoveUpTransition(ioA0)

    def doIt() = {
      mState.transitionWith(List(
        InitiativeTrackerUpdateStep(ioA0, InitiativeAction.MoveUp),
        MoveBeforeFirstStep(ioA0),
        SetRobinStep(ioA0))) returns mState2

      val nState = transition.transition(mLF, mState)
      (there was one(mState).transitionWith(List(
        InitiativeTrackerUpdateStep(ioA0, InitiativeAction.MoveUp),
        MoveBeforeFirstStep(ioA0),
        SetRobinStep(ioA0)))) and
        (nState must_== mState2)

    }
  }

  case class moveBefore() extends TestBase {
    val transition = MoveBeforeTransition(ioA0, ioB0)

    val mRule = mock[CombatState.Rules]
    mState.rules returns mRule


    val mOrder = mock[InitiativeOrder]
    val mOrderLens: Lens[CombatState, InitiativeOrder] = Lens(x => mOrder, (s, t) => s)
    mLF.orderLens returns mOrderLens

    def illegalMove() = {
      mRule.canMoveBefore(mState, ioA0, ioB0) returns false
      (transition.transition(mLF, mState) must throwAn(new IllegalActionException("Cant move " + ioA0 + " before " + ioB0))) and
        (there was one(mRule).canMoveBefore(mState, ioA0, ioB0))
    }

    def validMoveOfFirst() = {
      mRule.canMoveBefore(mState, ioA0, ioB0) returns true
      mOrder.nextUp returns Some(ioA0)

      mState.transitionWith(List(
        RotateRobinStep,
        MoveBeforeOtherStep(ioA0, ioB0))) returns mState2

      val nState = transition.transition(mLF, mState)
      (there was one(mState).transitionWith(List(
        RotateRobinStep,
        MoveBeforeOtherStep(ioA0, ioB0)))) and
        (nState must_== mState2)
    }

    def validMoveOfNotFirst() = {
      mRule.canMoveBefore(mState, ioA0, ioB0) returns true
      mOrder.nextUp returns Some(InitiativeOrderID(CombatantID("1"), 0))

      mState.transitionWith(List(MoveBeforeOtherStep(ioA0, ioB0))) returns mState2

      val nState = transition.transition(mLF, mState)
      (there was one(mState).transitionWith(List(MoveBeforeOtherStep(ioA0, ioB0)))) and
        (nState must_== mState2)
    }
  }

}