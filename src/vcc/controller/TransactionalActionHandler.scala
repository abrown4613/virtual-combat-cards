//$Id$
package vcc.controller

import vcc.controller.transaction.Transaction
import vcc.controller.actions.TransactionalAction

/**
 * Provide a way to handle Transactional Actions. This handler is bound to a context and is 
 * responsible for handling game system specific TransactionalActions, wrapping them in the 
 * proper transaction.
 */
abstract class TransactionalActionHandler[C](context:C) extends PartialFunction[(Transaction,TransactionalAction),Unit] {
  
  /**
   * This is the transaction holder, should only be used internally
   */
  protected implicit var trans:Transaction=null
  
  /**
   * Handler partial function, should be define in implementation.
   */
  protected val handler:PartialFunction[TransactionalAction,Unit] 
 
  /**
   * Determines if the Handler is defined for a specific action, transaction parameter will be ignored.
   */
  def isDefinedAt(p:(Transaction,TransactionalAction)) = handler.isDefinedAt(p._2)

  /**
   * Call internal handler, but first set the transaction and then unset the transaction
   */
  def apply(p:(Transaction,TransactionalAction)):Unit = { 
    trans=p._1
    val r=handler.apply(p._2)
    trans=null
    r    
  }
  
  def publish(changes:Seq[transaction.ChangeNotification],to:TrackerResponseBuffer)
}

abstract class QueryActionHandler[C](context:C) extends PartialFunction[(TrackerResponseBuffer,actions.QueryAction),Unit] {
  protected var obs:TrackerResponseBuffer=null
  protected val query:PartialFunction[actions.QueryAction,Unit]
  
  def isDefinedAt(qa:(TrackerResponseBuffer,actions.QueryAction)) = if(query!=null) query.isDefinedAt(qa._2) else false
  
  def apply(qa:(TrackerResponseBuffer,actions.QueryAction)) = {
    obs=qa._1
    val r=query(qa._2)
    obs=null
    r
  }
}

