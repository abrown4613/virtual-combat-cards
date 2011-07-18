/*
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
 * generate TransactionalAction. All Ruling have answer/question type.
 */
trait Ruling {

  def isValidDecision(decision: Decision[_]): Boolean = (decision.ruling == this && decisionValidator(decision))

  protected def decisionValidator(decision: Decision[_]): Boolean
}

/**
 * Decision is a simple class to collect the Decision on a Ruling and it's original ruling.
 */
abstract class Decision[R <: Ruling] {
  /**
   * Question to which this specific answer is an answer.
   */
  val ruling: R

}

/**
 * RulingDecisionHandler are used to generate some content of type R based on a given decision.
 */
trait RulingDecisionHandler[R] {

  /**
   * Given a Decision this function should return a valid R.
   */
  def processDecision(decision: Decision[_ <: Ruling]): R

}

/**
 * A PendingRuling is a Ruling that is awaiting a Decision. It also include a callback RulingDecisionHandler that will
 * be called if a valid Decision is provided for that Ruling.
 * @param ruling Ruling that is pending
 * @param callback Function to be called if the Decision is a valid answer for the Ruling
 */
class PendingRuling[R](val ruling: Ruling, callback: RulingDecisionHandler[R]) {

  /**
   * Constructor that accepts a function instead of a RulingDecisionHandler.
   * @param ruling Ruling that is pending
   * @param callback Function that works as the RulingDecisionHandler.processDecision
   */
  def this(ruling: Ruling, callback: Decision[_ <: Ruling] => R) = this (ruling, new RulingDecisionHandler[R] {
    def processDecision(decision: Decision[_ <: Ruling]): R = callback(decision)
  })

  /**
   * Constructor used for ruling that include their own RulingDecisionHandler.
   * @param ruling Pending ruling which must implement the RulingDecisionHandler
   */
  def this(ruling: Ruling with RulingDecisionHandler[R]) = this (ruling, ruling)

  /**
   * Processes a given decision against the pending ruling. It will verify that the Decision is a valid answer for the
   * Ruling.
   * @param decision The decision to which we want to generate the resulting.
   * @return None if the {@code decision} is not valid, Some(r) if the ruling is valid. r will be the result of calling
   * callback RulingDecisionHandler's processDecision.
   */
  def processDecision(decision: Decision[_ <: Ruling]): Option[R] = {
    if (ruling.isValidDecision(decision)) {
      Some(callback.processDecision(decision))
    } else
      None
  }
}


/**
 *   CommandSource represents the entity that sent the TransactionalAction to the Tracker. It will be used
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
   * @param context What action is being executed when we do these queries
   * @param rulings Item that require ruling by the CommandSource, these can be seen as questions that need answers.
   * @return The return must include a single Decision for each Ruling in the order they were provided in the
   * {@code ruling} parameter. 
   */
  def provideDecisionsForRulings(context: TransactionalAction, rulings: List[Ruling]): List[Decision[_ <: Ruling]]
}
