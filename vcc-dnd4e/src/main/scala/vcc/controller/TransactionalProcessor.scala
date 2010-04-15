/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.controller

import vcc.controller.transaction.Transaction
import vcc.controller.message.TransactionalAction

/**
 * This exception should be thrown when the action requested cant be executed
 * @param msg Reason fro the Illegality
 */
class IllegalActionException(msg: String) extends Exception(msg)

/**
 * Thrown when no handler deals with this action 
 */
class UnhandledActionException(action: TransactionalAction) extends Exception()

/**
 * TransactionalProcessor is a container for a set of PartialFunctions that 
 * process an action. They are all called in sequence, and should be defined via
 * traits in order to access context and transaction fields.
 */
abstract class TransactionalProcessor[C](val context: C) extends TrackerController {
  protected val msgQueue = new scala.collection.mutable.Queue[TransactionalAction]

  /**
   * This is the transaction holder, should only be used internally
   */
  protected implicit var trans: Transaction = null

  /**
   * All commands must have a source, this will allow handlers
   * to request information from the source
   */
  protected var source: CommandSource = null

  /**
   * A list of handler PartialFunctions that should be added in traits.
   */
  private var handlers: List[PartialFunction[TransactionalAction, Unit]] = Nil

  def addHandler(handler: PartialFunction[TransactionalAction, Unit]) {
    handlers = handlers ::: List(handler)
  }

  def rewriteEnqueue(action: TransactionalAction) {
    msgQueue.enqueue(action)
  }

  /**
   * Call internal handlers, but first set the transaction and then unset the transaction
   */
  def dispatch(transaction: Transaction, source: CommandSource, action: TransactionalAction): Unit = {
    rewriteEnqueue(action)
    trans = transaction
    this.source = source
    try {
      while (!msgQueue.isEmpty) {
        val msg = msgQueue.dequeue
        var handled = false
        for (hndl <- handlers) {
          if (hndl.isDefinedAt(msg)) {
            handled = true
            hndl.apply(msg)
          }
        }
        if (!handled) throw new UnhandledActionException(msg)
      }
    } catch {
      // We had an exception, flush message buffer to avoid leaving trash messages
      case e =>
        msgQueue.clear()
        throw e
    }
    trans = null
  }

}