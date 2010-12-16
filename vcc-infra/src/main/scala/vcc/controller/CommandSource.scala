/**
 *    Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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

import message.TransactionalAction

/**
 * A ruling is some aspect of the system that cannot be decided by the computer and need some form of human decisions.
 * It is used to get information from the user prior to executing actions. It is assumed that a Ruling will itself
 * generate TransactionalAction. All Ruling have answer/decision type.
 */
trait Ruling[A] {

  /**
   * This method must generate a list actions that based on a given decision. The list may be empty, but may not be null.
   * For any given {@code decision} any evocation of this method should produce the same list of actions (idempotent).
   * @param decision The value of the decision
   * @return a List of action that represent the effects of a Ruling on the system.
   */
  protected[controller] def generateActions(decision: A): List[TransactionalAction]

  /**
   * Helper function that will return a Decision wrapping this ruling and a valid answer to it.
   */
  def makeDecision(decision: A): Decision[A] = new Decision(decision, this)
}

/**
 * Decision is a simple class to collect the Decision on a Ruling and it's original ruling.
 */
class Decision[A](val decision: A, val decisionFor: Ruling[A]) {

  /**
   * This method will access the Ruling's implementation of the action generator, and produce a ser
   */
  def generateRulingActions(): List[TransactionalAction] = decisionFor.generateActions(decision)
}

/**
 * CommandSource represents the entity that sent the TransactionalAction to the Tracker. It will be used
 * to report success or failure of the execution of an action, and to allow the TransactionalProcessors to query the
 * source for ruling on things the tracker cannot resolve by itself.
 */
trait CommandSource {

  /**
   * This method is invoked to indicate that a given actions was successfully executed.
   * @param msg Information on what was executed (normally the TransactionalAction message)
   * @param producedChanges Indicates if the Transaction contains changes to the current state of the system.
   */
  def actionCompleted(msg: String, producedChanges: Boolean)

  /**
   * Indicates that the action being executed failed to complete, normally due to exceptions.
   * @param reason Description of the problem that caused the action to fail.
   */
  def actionCancelled(reason: String)

  /**
   * This method is used to query the CommandSource for ruling on issues that the tracker cannot resolve by itself.
   * @param rulings Item that require ruling by the CommandSource, these can be seen as questions that need answers.
   * @return The return must include a single Decision for each Ruling in the order they were provided in the
   * {@code ruling} parameter. 
   */
  def provideDecisionsForRulings(rulings: List[Ruling[_]]): List[Decision[_]]
}
