/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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

import actors.{DaemonActor, Actor}
import vcc.tracker.Tracker.{Controller, Observer}

object Tracker {

  trait Observer[S] {
    def stateUpdated(n: S)
  }

  trait Controller[S] {
    def clearHistory()

    def redo(): Option[S]

    def undo(): Option[S]

    def setInitialState(state: S)

    def dispatchAction(action: Action[S], rulingProvider: RulingProvider[S]): Option[S]
  }

}

class Tracker[S](controller: Controller[S]) {

  private case class AddObserver(observer: Observer[S])

  private case class NotifyObserver(newState: S)

  private case object Redo

  private case object Undo

  private case object ClearHistory

  private case class Dispatch(action: Action[S], rulingProvider: RulingProvider[S])

  private case class Initialize(state: S)

  private val observerRelay: Actor = new ObserverActor()
  private val tracker: Actor = new TrackerActor(controller)
  observerRelay.start()
  tracker.start()

  private[tracker] def notifyObservers(newState: S) {
    observerRelay ! NotifyObserver(newState)
  }

  private class ObserverActor extends DaemonActor {
    private var observers: List[Observer[S]] = Nil

    def act() {
      loop {
        react {
          case AddObserver(observer) => registerObserver(observer)
          case NotifyObserver(newState) => notifyObservers(newState)
          case s =>
        }
      }
    }

    private def registerObserver(observer: Tracker.Observer[S]) {
      observers = observer :: observers
    }

    private def notifyObservers(newState: S) {
      observers.foreach(notifyObserverCaptureExceptions(_, newState))
    }

    private def notifyObserverCaptureExceptions(observer: Observer[S], newState: S) {
      try {
        observer.stateUpdated(newState)
      } catch {
        case s: Exception =>
      }
    }
  }

  private class TrackerActor(controller: Controller[S]) extends DaemonActor {
    def act() {
      loop {
        react {
          case Redo => notifyObserversIfStateDefined(controller.redo())
          case Undo => notifyObserversIfStateDefined(controller.undo())
          case ClearHistory => controller.clearHistory()
          case Dispatch(action, rulingProvider) =>
            notifyObserversIfStateDefined(controller.dispatchAction(action, rulingProvider))
          case Initialize(state) => initializeState(state)
          case _ =>
        }
      }
    }

    private def initializeState(initialState: S) {
      controller.setInitialState(initialState)
      notifyObservers(initialState)
    }

    private def notifyObserversIfStateDefined(newState: Option[S]) {
      if (newState.isDefined)
        notifyObservers(newState.get)
    }
  }

  def addObserver(observer: Observer[S]) {
    observerRelay ! AddObserver(observer)
  }

  def initializeState(initialState: S) {
    tracker ! Initialize(initialState)
  }

  def undo() {
    tracker ! Undo
  }

  def redo() {
    tracker ! Redo
  }

  def clearHistory() {
    tracker ! ClearHistory
  }

  def dispatchAction(action: Action[S], rulingProvider: RulingProvider[S]) {
    tracker ! Dispatch(action, rulingProvider)
  }
}