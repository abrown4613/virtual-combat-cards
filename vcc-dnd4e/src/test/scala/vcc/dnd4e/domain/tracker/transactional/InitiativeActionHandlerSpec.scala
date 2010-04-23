/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.transactional


import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import vcc.dnd4e.domain.tracker.common.Command._
import vcc.dnd4e.domain.tracker.common._
import collection.mutable.Queue
import org.specs.mock.Mockito
import vcc.controller.transaction.Transaction
import vcc.controller.message.TransactionalAction
import vcc.controller.{IllegalActionException, UnhandledActionException, CommandSource}
import vcc.dnd4e.domain.tracker.common.InitiativeTracker.{action, state}

@RunWith(classOf[JUnitSuiteRunner])
class InitiativeActionHandlerTest extends JUnit4(InitiativeActionHandlerSpec)

object InitiativeActionHandlerSpec extends Specification with Mockito {
  class PartialCombatController(rules: CombatStateRules, state: CombatState, queue: Queue[TransactionalAction])
          extends AbstractCombatController(rules, state, queue)
                  with InitiativeActionHandler

  val combA = CombatantID("A")
  val combB = CombatantID("B")
  val combC = CombatantID("C")
  val ioa = InitiativeOrderID(combA, 0)
  val iob = InitiativeOrderID(combB, 0)
  val ioc = InitiativeOrderID(combC, 0)

  val sQueue = spy(new Queue[TransactionalAction]())

  val mRules = mock[CombatStateRules]
  val mOrder = mock[InitiativeOrder]
  val mRoster = mock[CombatantRoster]
  val mMeta = mock[CombatMetaData]

  val rState = new CombatState(mOrder, mRoster, mMeta)

  val mSource = mock[CommandSource]
  val aController = new PartialCombatController(mRules, rState, sQueue)

  "InitiativeActionHandler rewrite" should {

    "expand Delay to StartRound and Delay" in {
      val mTracker = mock[InitiativeTracker]
      mTracker.orderID returns ioa
      mOrder.isDefinedAt(ioa) returns true
      mOrder.initiativeTrackerFor(ioa) returns mTracker
      try {
        aController.dispatch(new Transaction(), mSource, InitiativeAction(ioa, InitiativeTracker.action.Delay))
      } catch {
        case _ => // We are not concerned yet
      }

      there was one(sQueue).+=(InternalInitiativeAction(ioa, InitiativeTracker.action.StartRound))
      there was one(sQueue).+=(InternalInitiativeAction(ioa, InitiativeTracker.action.Delay))
    }

    "expand Ready to Ready and EndRound" in {
      val mTracker = mock[InitiativeTracker]
      mTracker.orderID returns ioa
      mOrder.isDefinedAt(ioa) returns true
      mOrder.initiativeTrackerFor(ioa) returns mTracker
      try {
        aController.dispatch(new Transaction(), mSource, InitiativeAction(ioa, InitiativeTracker.action.Ready))
      } catch {
        case _ => // We are not concerned yet
      }

      there was one(sQueue).+=(InternalInitiativeAction(ioa, InitiativeTracker.action.Ready)) then
              one(sQueue).+=(InternalInitiativeAction(ioa, InitiativeTracker.action.EndRound))
    }

    "convert remainder if in order" in {
      val data: List[InitiativeTracker.action.Value] = List(
        InitiativeTracker.action.StartRound, InitiativeTracker.action.EndRound,
        InitiativeTracker.action.ExecuteReady, InitiativeTracker.action.MoveUp)

      val mTracker = mock[InitiativeTracker]
      mTracker.orderID returns ioa

      mOrder.isDefinedAt(any[InitiativeOrderID]) returns true
      mOrder.initiativeTrackerFor(any[InitiativeOrderID]) returns mTracker

      for (action <- data) {
        try {
          aController.dispatch(new Transaction(), mSource, InitiativeAction(ioa, action))
        } catch {
          case _ => // We are not concerned yet
        }
        there was one(sQueue).+=(InternalInitiativeAction(ioa, action))
      }
    }

    "throw exception if initiativeOrder is no present" in {
      mOrder.isDefinedAt(ioa) returns false
      aController.dispatch(new Transaction(), mSource, InitiativeAction(ioa, InitiativeTracker.action.StartRound)) must
              throwA[UnhandledActionException]
    }
  }

  "InitiativeActionHandler" should {
    "throw exception if StartRound is not allowed" in {
      mRules.canInitiativeOrderPerform(rState, ioc, action.StartRound) returns false
      setMockInitiativeTracker(ioc, state.Acting)
      mOrder.robinHeadInitiativeTracker() returns InitiativeTracker(ioa, 0, state.Acting)

      val trans = new Transaction()
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.StartRound)) must throwAn[IllegalActionException]

      there was one(mRules).canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.StartRound)
    }

    "throw exception if not EndRound if not allowed" in {
      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.EndRound) returns false
      setMockInitiativeTracker(ioc, state.Waiting)
      mOrder.robinHeadInitiativeTracker() returns InitiativeTracker(ioa, 0, state.Acting)

      val trans = new Transaction()
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.EndRound)) must throwAn[IllegalActionException]

      there was one(mRules).canInitiativeOrderPerform(rState, ioc, action.EndRound)
    }

    "start round if allowed" in {
      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.StartRound) returns true
      setMockInitiativeTracker(ioc, state.Waiting)
      val baseIT = InitiativeTracker(ioc, 0, state.Waiting)
      val updatedIT = baseIT.transform(baseIT, action.StartRound)
      mOrder.robinHeadInitiativeTracker() returns InitiativeTracker(ioc, 0, state.Waiting)

      val trans = new Transaction()
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.StartRound))

      there was one(mOrder).updateInitiativeTrackerFor(ioc, updatedIT)(trans)
    }

    "end round and rotate if allowed" in {
      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.EndRound) returns true
      setMockInitiativeTracker(ioc, state.Acting)
      val baseIT = InitiativeTracker(ioc, 0, state.Acting)
      val updatedIT = baseIT.transform(baseIT, action.EndRound)
      mOrder.robinHeadInitiativeTracker() returns InitiativeTracker(ioc, 0, state.Acting) thenReturns InitiativeTracker(iob, 0, state.Waiting)

      mockNextAsNotDead(combB)

      val trans = new Transaction()
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.EndRound))

      there was one(mOrder).updateInitiativeTrackerFor(ioc, updatedIT)(trans)
      there was one(mOrder).rotate()(trans)
    }

    "auto advance next guy if he is dead" in {
      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.EndRound) returns true
      setMockInitiativeTracker(ioc, state.Acting)
      mOrder.robinHeadInitiativeTracker() returns InitiativeTracker(ioc, 0, state.Acting) thenReturns InitiativeTracker(iob, 0, state.Waiting)

      mockNextAsDead(combB)

      val trans = new Transaction()
      // We expect and exception because of incomplete mocking
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.EndRound)) must throwAn[IllegalActionException]

      there was one(sQueue).+=(InternalInitiativeAction(iob, InitiativeTracker.action.StartRound)) then
              one(sQueue).+=(InternalInitiativeAction(iob, InitiativeTracker.action.EndRound))
    }

    "auto advance dead after a first delays" in {
      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.Delay) returns true
      setMockInitiativeTracker(ioc, state.Acting)
      mOrder.robinHeadInitiativeTracker() returns InitiativeTracker(ioc, 0, state.Acting) thenReturns InitiativeTracker(iob, 0, state.Waiting)

      mockNextAsDead(combB)

      val trans = new Transaction()
      // We expect and exception because of incomplete mocking
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.Delay)) must throwAn[IllegalActionException]

      there was one(sQueue).+=(InternalInitiativeAction(iob, InitiativeTracker.action.StartRound)) then
              one(sQueue).+=(InternalInitiativeAction(iob, InitiativeTracker.action.EndRound))
    }

    "auto advance next guy if he is dead and delaying" in {
      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.EndRound) returns true
      setMockInitiativeTracker(ioc, state.Acting)
      mOrder.robinHeadInitiativeTracker() returns InitiativeTracker(ioc, 0, state.Acting) thenReturns InitiativeTracker(iob, 0, state.Delaying)

      mockNextAsDead(combB)

      val trans = new Transaction()
      // We expect and exception because of incomplete mocking
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.EndRound)) must throwAn[IllegalActionException]

      there was one(sQueue).+=(InternalInitiativeAction(iob, InitiativeTracker.action.EndRound)) then
              one(sQueue).+=(InternalInitiativeAction(iob, InitiativeTracker.action.StartRound)) then
              one(sQueue).+=(InternalInitiativeAction(iob, InitiativeTracker.action.EndRound))
    }

    "auto advance end next guy if he is dead and readied" in {
      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.EndRound) returns true
      setMockInitiativeTracker(ioc, state.Acting)
      mOrder.robinHeadInitiativeTracker() returns InitiativeTracker(ioc, 0, state.Acting) thenReturns InitiativeTracker(iob, 0, state.Delaying)

      mockNextAsDead(combB)

      val trans = new Transaction()
      // We expect and exception because of incomplete mocking
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.EndRound)) must throwAn[IllegalActionException]

      there was one(sQueue).+=(InternalInitiativeAction(iob, InitiativeTracker.action.StartRound)) then
              one(sQueue).+=(InternalInitiativeAction(iob, InitiativeTracker.action.EndRound))
    }
  }

  "InitiativeActionHandler on Delay actions (internal actions)" should {
    // This will test Delay and MoveUp

    "throw exception if delay is not allowed" in {
      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.Delay) returns false
      setMockInitiativeTracker(ioc, state.Waiting)
      mOrder.robinHeadInitiativeTracker() returns InitiativeTracker(ioa, 0, state.Waiting)

      val trans = new Transaction()
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.Delay)) must throwAn[IllegalActionException]

      there was one(mRules).canInitiativeOrderPerform(rState, ioc, action.Delay)
    }

    "rotate after delay" in {
      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.Delay) returns true
      setMockInitiativeTracker(ioc, state.Acting)
      val baseIT = InitiativeTracker(ioc, 0, state.Acting)
      val updatedIT = baseIT.transform(baseIT, action.Delay)
      mOrder.robinHeadInitiativeTracker() returns InitiativeTracker(ioc, 0, state.Acting) thenReturns InitiativeTracker(iob, 0, state.Waiting)

      mockNextAsNotDead(combB)

      val trans = new Transaction()
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.Delay))

      there was one(mOrder).updateInitiativeTrackerFor(ioc, updatedIT)(trans)
      there was one(mOrder).rotate()(trans)
    }

    "throw exception if MoveUp is not allowed" in {
      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.MoveUp) returns false
      setMockInitiativeTracker(ioc, state.Delaying)
      mOrder.robinHeadInitiativeTracker() returns InitiativeTracker(ioa, 0, state.Acting)

      val trans = new Transaction()
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.MoveUp)) must throwAn[IllegalActionException]

      there was one(mRules).canInitiativeOrderPerform(rState, ioc, action.MoveUp)
    }


    "move before first, make it the first in the case of a legal MoveUp" in {
      //Delay move up must change tracker and move to before first
      val firstIT = InitiativeTracker(ioa, 0, state.Waiting)
      val baseIT = InitiativeTracker(ioc, 0, state.Delaying)
      val updatedIT = baseIT.transform(firstIT, action.MoveUp)

      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.MoveUp) returns true
      setMockInitiativeTracker(ioc, state.Delaying)
      mOrder.robinHeadInitiativeTracker() returns firstIT

      val trans = new Transaction()
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.MoveUp))

      there was one(mOrder).updateInitiativeTrackerFor(ioc, updatedIT)(trans)
      there was one(mRules).canInitiativeOrderPerform(rState, ioc, action.MoveUp)
      there was one(mOrder).moveBefore(ioc, ioa)(trans) then
              one(mOrder).setRobinHead(ioc)(trans)
    }
  }

  "InitiativeActionHandler on Ready actions" should {

    // This will only look at InternalInitiativeAction, which means the End Round is handled by the rewrite
    "throw exception if Ready is not allowed" in {
      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.Ready) returns false
      setMockInitiativeTracker(ioc, state.Waiting)
      mOrder.robinHeadInitiativeTracker() returns InitiativeTracker(ioa, 0, state.Waiting)

      val trans = new Transaction()
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.Ready)) must throwAn[IllegalActionException]

      there was one(mRules).canInitiativeOrderPerform(rState, ioc, action.Ready)
    }

    "only Ready if rules allow" in {
      val baseIT = InitiativeTracker(ioc, 0, state.Acting)
      val updatedIT = baseIT.transform(baseIT, action.Ready)

      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.Ready) returns true
      setMockInitiativeTracker(ioc, state.Acting)
      mOrder.robinHeadInitiativeTracker() returns baseIT

      val trans = new Transaction()
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.Ready))

      there was one(mRules).canInitiativeOrderPerform(rState, ioc, action.Ready) then
              one(mOrder).updateInitiativeTrackerFor(ioc, updatedIT)(trans)
    }

    "end round and rotate when Readying" in {
      // This is a special case of end round, it should be called by the rewrite
      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.EndRound) returns true
      setMockInitiativeTracker(ioc, state.Readying)
      val baseIT = InitiativeTracker(ioc, 0, state.Readying)
      val updatedIT = baseIT.transform(baseIT, action.EndRound)
      mOrder.robinHeadInitiativeTracker() returns InitiativeTracker(ioc, 0, state.Acting) thenReturns InitiativeTracker(iob, 0, state.Waiting)

      mockNextAsNotDead(combB)

      val trans = new Transaction()
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.EndRound))

      there was one(mOrder).updateInitiativeTrackerFor(ioc, updatedIT)(trans)
      there was one(mOrder).rotate()(trans)
    }

    "move combatant before acting on when Executing readied action" in {
      //Move back to waiting, and move before first, no change in head
      val firstIT = InitiativeTracker(ioa, 0, state.Acting)
      val baseIT = InitiativeTracker(ioc, 0, state.Ready)
      val updatedIT = baseIT.transform(firstIT, action.ExecuteReady)

      mRules.canInitiativeOrderPerform(rState, ioc, InitiativeTracker.action.ExecuteReady) returns true
      setMockInitiativeTracker(ioc, state.Ready)
      mOrder.robinHeadInitiativeTracker() returns firstIT

      val trans = new Transaction()
      aController.dispatch(trans, mSource, InternalInitiativeAction(ioc, action.ExecuteReady))

      there was one(mRules).canInitiativeOrderPerform(rState, ioc, action.ExecuteReady)
      there was one(mOrder).updateInitiativeTrackerFor(ioc, updatedIT)(trans)
      there was one(mOrder).moveBefore(ioc, ioa)(trans)
    }
  }

  "InitiativeActionHandler handling a MoveBefore" should {

    "fail if not allowed" in {
      mRules.canMoveBefore(rState, ioa, ioa) returns false
      mRules.canMoveBefore(rState, ioa, iob) returns false
      setMockInitiativeTracker(ioa, state.Waiting)
      setMockInitiativeTracker(iob, state.Waiting)

      aController.dispatch(new Transaction(), mSource, MoveBefore(ioa, iob)) must throwA[IllegalActionException]
    }

    "fail if either InitiativeOrderID is not in roster" in {
      mOrder.isDefinedAt(ioa) returns true
      mOrder.isDefinedAt(iob) returns false
      aController.dispatch(new Transaction(), mSource, MoveBefore(ioa, iob)) must throwA[UnhandledActionException]

      mOrder.isDefinedAt(ioa) returns false
      mOrder.isDefinedAt(iob) returns true
      aController.dispatch(new Transaction(), mSource, MoveBefore(ioa, iob)) must throwA[UnhandledActionException]
    }

    "move if it is allowed" in {
      //This means that move befora acting will place you in last
      mRules.canMoveBefore(rState, ioa, iob) returns true
      setMockInitiativeTracker(ioa, state.Waiting)
      setMockInitiativeTracker(iob, state.Waiting)
      mOrder.getIDsInOrder returns List(iob, ioa, ioc)

      val trans = new Transaction()
      aController.dispatch(trans, mSource, MoveBefore(ioa, iob))

      there was one(mOrder).moveBefore(ioa, iob)(trans)
      there was one(mOrder).getIDsInOrder
      there was no(mOrder).setRobinHead(ioa)(trans)
      there was no(mOrder).setRobinHead(iob)(trans)
      there was no(mOrder).setRobinHead(ioc)(trans)
    }

    "move to second if moving non acting first to another position" in {
      mRules.canMoveBefore(rState, ioa, iob) returns true
      setMockInitiativeTracker(ioa, state.Waiting)
      setMockInitiativeTracker(iob, state.Waiting)
      mOrder.getIDsInOrder returns List(ioa, ioc, iob)

      val trans = new Transaction()

      aController.dispatch(trans, mSource, MoveBefore(ioa, iob))

      there was one(mOrder).moveBefore(ioa, iob)(trans)
      there was one(mOrder).setRobinHead(ioc)(trans)
    }
  }

  def setMockInitiativeTracker(io: InitiativeOrderID, state: InitiativeTracker.state.Value) {
    mOrder.isDefinedAt(io) returns true
    mOrder.initiativeTrackerFor(io) returns InitiativeTracker(io, 0, state)
  }

  def mockNextAsDead(combId: CombatantID) {
    val mockNext = mock[Combatant]
    mockNext.health returns HealthTracker(0, 0, 3, 0, MinionHealthDefinition())
    mockNext.health.status must_== HealthTracker.Status.Dead
    mRoster.combatant(combId) returns mockNext
  }

  def mockNextAsNotDead(combId: CombatantID) {
    val mockNext = mock[Combatant]
    mockNext.health returns HealthTracker(1, 0, 0, 0, MinionHealthDefinition())
    mockNext.health.status must_!= HealthTracker.Status.Dead
    mRoster.combatant(combId) returns mockNext
  }

}