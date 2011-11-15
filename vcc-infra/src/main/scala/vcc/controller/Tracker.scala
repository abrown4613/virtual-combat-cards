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
package vcc.controller

import scala.actors.Actor
import vcc.controller.transaction._
import vcc.controller.message.TransactionalAction

/**
 * Tracker actor handles the core logic for the event dispatch loop. It controls
 * transactions (start, end and clearing the log), undo/redo, and observer registration.
 * It will dispatch actions and query to the controller handlers, and will gather return
 * data to be passed on to the observer.
 * @param controller Action and query logic controller
 * @param _tlog The object to contain the Transaction
 */
class Tracker(controller: TrackerController, _tlog: TransactionLog[TransactionalAction])
  extends Actor with TransactionChangePublisher {
  def this(controller: TrackerController) = this (controller, new TransactionLog[TransactionalAction]())

  private val logger = org.slf4j.LoggerFactory.getLogger("infra")

  def isStartupComplete = true

  private var observers: List[Actor] = Nil

  /**
   * Publish changes to the observers
   */
  def publishChange(changes: Seq[ChangeNotification]) {
    val msg = controller.publish(changes)
    for (obs <- observers) obs ! msg
  }

  def act() {
    loop {
      react {
        case message.AddObserver(obs) =>
          observers = obs :: observers

        case message.Command(from, action) =>
          val trans = new Transaction()
          try {
            controller.dispatch(trans, from, action)
            trans.commit(this)
            if (!trans.isEmpty) {
              _tlog.store(action, trans)
            }
            from.actionCompleted(action.description, !trans.isEmpty)
          } catch {
            case e =>
              logger.warn("An exception occured while processing: " + action, e)
              logger.warn("Rolling back transaction")
              if (trans.state == Transaction.state.Active) trans.cancel()
              from.actionCancelled(e.getMessage)
          }
        case message.Undo() =>
          try {
            _tlog.rollback(this)
          } catch {case s: TransactionLogOutOfBounds =>}
        case message.Redo() =>
          try {
            _tlog.rollforward(this)
          } catch {case s: TransactionLogOutOfBounds =>}
        case message.ClearTransactionLog() =>
          _tlog.clear()

        case s =>
          logger.warn("Error: Tracker can't handle this event: " + s)
      }
    }
  }
}

object Tracker {
  def initialize(tc: TrackerController): Tracker = {
    val tracker = new Tracker(tc)
    tracker.start()
    tracker
  }

}