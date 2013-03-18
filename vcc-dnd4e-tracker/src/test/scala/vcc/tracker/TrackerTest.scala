/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.tracker

import helper.State
import org.specs2.SpecificationWithJUnit
import util.Random
import java.lang.Thread
import concurrent.SyncVar
import org.specs2.mock.Mockito

class TrackerTest extends SpecificationWithJUnit {

  private class ThreadedObserver[S] extends Tracker.Observer[S] {
    private val state = new SyncVar[S]

    def getObservedState: Option[S] = state.get(70)

    def stateUpdated(newState: S) {
      state.put(newState)
    }
  }

  private class CrashObserver[S] extends Tracker.Observer[S] {
    def stateUpdated(n: S) {
      throw new Exception("Boom!")
    }
  }

  def is =
    sequential ^
      "ThreadedObserver" ^
      "  get and answer in time out range" ! threadObserver().answerComeWithinTimeLimit ^
      "  fails if answer does not come quick enough" ! threadObserver().answerDoesNotComeWithTimeLimit ^
      endp ^
      "Tracker notification" ^
      "  notify single observer" ! notificationCases().roundTripToSingleObserver ^
      "  nofify two observer" ! notificationCases().roundTripToTwoObserver ^
      "  should not crash on misbehaved observer" ! notificationCases().misbehavedObserverMustNotCrashObserver ^
      endp ^
      "Tracker action handling" ^
      "  set initial state of tracker" ! notificationCases().defineInitialState ^
      "  process undo with no return state " ! notificationCases().handleUndoWithNothingToBeDone ^
      "  process undo with state to send" ! notificationCases().handleUndoWithState ^
      "  process redo with no return state " ! notificationCases().handleRedoWithNothingToBeDone ^
      "  process redo with state to send" ! notificationCases().handleRedoWithState ^
      "  process clear log" ! notificationCases().handleClearLog ^
      "  process action dispatched with no state change" ! notificationCases().handleActionDispatchWithNoState ^
      "  process action dispatched with state change" ! notificationCases().handleActionDispatchWithStateChange ^
      endp ^
      end

  case class threadObserver() {
    private val to = new ThreadedObserver[Int]()
    private val n = Random.nextInt()

    def answerComeWithinTimeLimit = {
      fireUpdateAfterMillis(20)
      to.getObservedState must_== Some(n)
    }

    def answerDoesNotComeWithTimeLimit = {
      fireUpdateAfterMillis(120)
      to.getObservedState must_== None
    }

    private def fireUpdateAfterMillis(i: Int) {
      executeInOtherThread(i) {
        to.stateUpdated(n)
      }
    }
  }

  private case class notificationCases() extends Mockito {
    private val mockController = mock[Tracker.Controller[State]]
    private val tracker = new Tracker[State](mockController)
    private val observer = createAndRegisterThreadedObserver()
    private val mockAction = mock[Action[State]]
    private val mockRulingProvider = mock[RulingProvider[State]]
    private val n = Random.nextInt()

    def roundTripToSingleObserver = {
      executeInOtherThread(20)(tracker.notifyObservers(State(n)))
      observer.getObservedState must_== Some(State(n))
    }

    def roundTripToTwoObserver = {
      val observer2 = createAndRegisterThreadedObserver()
      executeInOtherThread(20)(tracker.notifyObservers(State(n)))
      (observer2.getObservedState must_== Some(State(n))) and
        (observer.getObservedState must_== Some(State(n)))
    }

    def misbehavedObserverMustNotCrashObserver = {
      tracker.addObserver(new CrashObserver[State])
      executeInOtherThread(20)(tracker.notifyObservers(State(n)))
      observer.getObservedState must_== Some(State(n))
    }

    private def createAndRegisterThreadedObserver(): ThreadedObserver[State] = {
      val observer = new ThreadedObserver[State]()
      tracker.addObserver(observer)
      observer
    }

    def defineInitialState = {
      executeInOtherThread(20)(tracker.initializeState(State(n)))
      observer.getObservedState must_== Some(State(n))
      there was one(mockController).setInitialState(State(n))
    }

    def handleUndoWithNothingToBeDone = {
      mockController.undo() returns None
      tracker.undo()
      (observer.getObservedState must_== None) and
        (there was one(mockController).undo())
    }

    def handleUndoWithState = {
      mockController.undo() returns Some(State(n))
      tracker.undo()
      (observer.getObservedState must_== Some(State(n))) and
        (there was one(mockController).undo())
    }

    def handleRedoWithNothingToBeDone = {
      mockController.redo() returns None
      tracker.redo()
      (observer.getObservedState must_== None) and
        (there was one(mockController).redo())
    }

    def handleRedoWithState = {
      mockController.redo() returns Some(State(n))
      tracker.redo()
      (observer.getObservedState must_== Some(State(n))) and
        (there was one(mockController).redo())
    }

    def handleClearLog = {
      tracker.clearHistory()
      observer.getObservedState
      there was one(mockController).clearHistory()
    }

    def handleActionDispatchWithNoState = {
      mockController.dispatchAction(mockAction, mockRulingProvider) returns None
      tracker.dispatchAction(mockAction, mockRulingProvider)

      (observer.getObservedState must_== None) and
        (there was one(mockController).dispatchAction(mockAction, mockRulingProvider))
    }

    def handleActionDispatchWithStateChange = {
      mockController.dispatchAction(mockAction, mockRulingProvider) returns Some(State(n))
      tracker.dispatchAction(mockAction, mockRulingProvider)

      (observer.getObservedState must_== Some(State(n))) and
        (there was one(mockController).dispatchAction(mockAction, mockRulingProvider))
    }
  }

  private def executeInOtherThread(blockPeriod: Long)(block: => Unit) {
    new Thread() {
      override def run() {
        Thread.sleep(blockPeriod)
        block
      }
    }.start()
  }
}