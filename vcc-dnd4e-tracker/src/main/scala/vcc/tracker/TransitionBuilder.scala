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
package vcc.tracker

import collection.mutable.ListBuffer
import org.slf4j.Logger

case class Transaction[S, A](inState: S, outState: S, command: A, events: List[StateTransition[S]])

trait TransitionBuilder[S, A] {

  def startTransaction(initialState: S, action: A)

  def startCommand(command: StateCommand[S])

  def addEvents(events: List[StateTransition[S]])

  def build(inState: S, outState: S): Transaction[S, A]
}

class AccumulatorTransitionBuilder[S, A] extends TransitionBuilder[S, A] {
  private val accumulated = new ListBuffer[StateTransition[S]]()
  private var action: Option[A] = None

  def startTransaction(initialState: S, action: A) {
    this.action = Option(action)
  }

  def startCommand(command: StateCommand[S]) {
    //Needed?
  }

  def addEvents(events: List[StateTransition[S]]) {
    accumulated appendAll events
  }

  def build(inState: S, outState: S): Transaction[S, A] = {
    Transaction(inState, outState, action.get, accumulated.toList)
  }
}

class DebugTransitionBuilder[S, A](delegate: TransitionBuilder[S, A], log: Logger) extends TransitionBuilder[S, A] {

  def startTransaction(initialState: S, action: A) {
    log.debug("Dispatching action: " + action)
    log.debug("Starting state: " + initialState)
    delegate.startTransaction(initialState, action)
  }

  def startCommand(command: StateCommand[S]) {
    log.debug("Processing command: " + command)
    delegate.startCommand(command)
  }

  def addEvents(events: List[StateTransition[S]]) {
    log.debug("Executed following events: " + events.mkString(", "))
    delegate.addEvents(events)
  }

  def build(inState: S, outState: S): Transaction[S, A] = {
    log.debug("State processing resulted in state: " + outState)
    delegate.build(inState, outState)
  }
}