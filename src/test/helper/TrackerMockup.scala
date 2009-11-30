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
package test.helper

import vcc.controller._
import vcc.controller.transaction._
import vcc.controller.message.TransactionalAction

/**
 * 
 */
trait ActionRecorder {
  def actionsExecuted:List[TransactionalAction]
}

/**
 * This is a TransactionaProcessor that accumulates internal actions
 * for scrutiny.
 */
trait ActionAccumulator[C] extends TransactionalProcessor[C]{
  self: TransactionalProcessor[C] =>
  
  var actionsProcessed:List[TransactionalAction]=Nil
  
  override def rewriteEnqueue(action:TransactionalAction) {
    actionsProcessed=Nil
    super.rewriteEnqueue(action)
  }
  
  addHandler {
    case msg =>
      actionsProcessed= actionsProcessed ::: List(msg)
  }
}

/**
 * Tracker mockup has several utility methods to help test action handlers and publishers. It is on
 * and actor so it works syncronly.
 */
class TrackerMockup[C](val controller:TrackerController[C]) extends TransactionChangePublisher {

  private var lastObserverMessage:Any = null
  
  def publishChange(changes:Seq[ChangeNotification]) {
    lastObserverMessage = controller.publish(changes)
  }
  
  /**
   * Dispatch and action and return the transaction not committed
   * @param otrans An open transaction, if null a new transaction will be created
   * @param msg The message to be dispatched
   * @return Open transaction, either otrans if it was valid, or a new one.
   */
  def dispatchWithoutCommit(otrans:Transaction, msg:TransactionalAction):Transaction = {
    val trans=if(otrans==null) new Transaction() else otrans
    controller.dispatch(trans,msg)
    trans
  }
  
  /**
   * Dispatch and action and commit transaction
   */
  def dispatch(msg:TransactionalAction) {
    val trans=new Transaction()
    controller.dispatch(trans,msg)
    trans.commit(this)
  }
  
  /**
   * Commit the transaction
   */
  def commitTransaction(trans:Transaction) = {
    trans.commit(this)
  }
  
  /**
   * Return the last TrackerResponseBuffer's message that would be sent
   */
  def lastChangeMessages = lastObserverMessage
}
