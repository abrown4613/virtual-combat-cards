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
package vcc.controller

import vcc.controller.transaction.Transaction
import vcc.controller.message.TransactionalAction
import scala.collection.mutable.Queue

/**
 * This exceptions is used to indicate that the CommandSource failed to return a decision for all rulings requested. This
 * may also occur if decisions are supplied in the wrong order.
 * @see TransactionalProcessor.queryCommandSource
 * @param ruling The missing ruling
 */
class MissingDecisionException(ruling: Ruling[_]) extends Exception("Missing Decision for Ruling: " + ruling)

/**
 * This exception should be thrown when the action requested cant be executed
 * @param msg Reason fro the Illegality
 */
class IllegalActionException(msg: String) extends Exception(msg)

/**
 * Thrown when no handler deals with this action 
 */
class UnhandledActionException(action: TransactionalAction) extends Exception("Cant handle: " + action)

/**
 * TransactionalProcessor is a container for a set of PartialFunctions that 
 * process an action. They are all called in sequence, and should be defined via
 * traits in order to access context and transaction fields.
 */
abstract class TransactionalProcessor[C](val context: C, aQueue: Queue[TransactionalAction]) extends TrackerController {
  private val msgQueue: Queue[TransactionalAction] = aQueue

  def this(context: C) = this (context, new Queue[TransactionalAction])

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

  /**
   * List of rewrite rules to be applied. These first match will be applied and all other ignored.
   */
  private var rewriteRules: List[PartialFunction[TransactionalAction, Seq[TransactionalAction]]] = Nil


  /**
   * List of Ruling search partial functions
   */
  private var rulingSearch: List[PartialFunction[TransactionalAction, List[Ruling[_]]]] = Nil

  /**
   *  Add a handler to the processor, all handlers that apply will be executed while
   * processing a TransactionalAction.
   * @param handler A partial function on TransactionalAction
   */
  protected def addHandler(handler: PartialFunction[TransactionalAction, Unit]) {
    handlers = handlers ::: List(handler)
  }

  /**
   * Adds a rewrite rule which can change a TransactionalAction into other actions according to need. These rules are
   * called when the message is dispatched. Only the first rule is invoked. TransactionalAction that have been generated
   * by the rule will NOT be processed again by rewrite rules. If no rule applies the original message is enqueued.
   * @param rule The rule that take a single TransactionalAction and rewrites to a sequence of actions.
   */
  protected def addRewriteRule(rule: PartialFunction[TransactionalAction, Seq[TransactionalAction]]) {
    rewriteRules = rewriteRules ::: List(rule)
  }

  /**
   * Adds a new action to the end of the primary action queue. This method bypasses rewrite mechanism.
   * @param action Action to be enqueued at first level.
   */
  protected def enqueueAction(action: TransactionalAction) {
    msgQueue.enqueue(action)
  }

  /**
   * Adds a ruling search to the processor, these partial function will be called to check if the current action being
   * dispatched requires ruling prior to it's execution. All searches will be applied and ruling generated added to a
   * single set of requests to the CommandSource. The Decisions will then be used to generate a set of TransactionalActions
   * that will be invoked prior to the original TransactionalAction. No further search for ruling will be applied on these
   * TransactionalActions generated.
   * @param search Partial function that will be checked for generation of Rulings 
   */
  protected def addRulingSearch(search: PartialFunction[TransactionalAction, List[Ruling[_]]]) {
    rulingSearch = rulingSearch ::: List(search)
  }


  /**
   * This helper function is used to quere CommandSource for decisions on rulings. It makes sure decisions come in the
   * correct order and for all rulings required, then generates all TransactionalAction related to the decisions
   * @param source CommandSource to be queried for decisions, this will normally map to the user interface.
   * @param rulings List of issues that need ruling
   * @return List of TransactionalAction generated concatenating each Decision.generateRulingActions all the decisions.
   */
  private[controller] def queryCommandSource(source: CommandSource, rulings: List[Ruling[_]]): List[TransactionalAction] = {
    val decisions = source.provideDecisionsForRulings(rulings)
    val zipped = rulings zip decisions
    for ((q, d) <- zipped) {
      if (q != d.decisionFor)
        throw new MissingDecisionException(q)
    }
    //Less answers than questions
    if (zipped.size < rulings.size) {
      throw new MissingDecisionException(rulings(zipped.size))
    }

    decisions.flatMap(d => d.generateRulingActions())
  }

  /**
   *  Call internal handlers, but first set the transaction and then unset the transaction
   */
  def dispatch(transaction: Transaction, source: CommandSource, action: TransactionalAction): Unit = {

    //Check for a valid rewrite and apply or else enqueue the unmodified message
    var rewrite = rewriteRules.find(rule => rule.isDefinedAt(action))
    if (rewrite.isDefined)
      for (act <- rewrite.get.apply(action)) msgQueue += act
    else
      msgQueue += action

    trans = transaction
    this.source = source
    try {
      while (!msgQueue.isEmpty) {
        //Expand actions to include ruled actions, this will also dequeue the main message
        val ruledActions: List[TransactionalAction] = {
          val msg = msgQueue.dequeue
          val rulings = rulingSearch.flatMap(search => if (search.isDefinedAt(msg)) search(msg) else Nil)
          if (!rulings.isEmpty)
            queryCommandSource(source, rulings) ::: List(msg)
          else
            List(msg)
        }

        //Loop through ruled actions
        for (message <- ruledActions) {
          var handled = false
          for (hndl <- handlers) {
            if (hndl.isDefinedAt(message)) {
              handled = true
              hndl.apply(message)
            }
          }
          if (!handled) throw new UnhandledActionException(message)
        }
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