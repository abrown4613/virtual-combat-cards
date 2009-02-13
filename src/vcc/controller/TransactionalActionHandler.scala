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
}

/**
 * Converts change notification to Messages to be sent to observers
 */
trait ChangePublisher[C] {
  
  /**
   * This method is used to convert ChangeNotifications to the messages that the observers
   * can handle.
   */
  def publish(context:C,changes:Seq[transaction.ChangeNotification],to:TrackerResponseBuffer)
}

/**
 * Handle QueryAction processing actions.
 */
abstract class QueryActionHandler[C](context:C) extends PartialFunction[(TrackerResponseBuffer,actions.QueryAction),Unit] {
  protected var obs:TrackerResponseBuffer=null
  
  /**
   * This is the PartialFunction that will it recieves a QueryAction and should send
   * results via the obs variable
   */
  protected val query:PartialFunction[actions.QueryAction,Unit]
  
  /**
   * Return whether or not this handles has interest in the action query. 
   */
  def isDefinedAt(qa:(TrackerResponseBuffer,actions.QueryAction)) = if(query!=null) query.isDefinedAt(qa._2) else false
  
  /**
   * Process the QueryTransaction and send result to buffer.
   */
  def apply(qa:(TrackerResponseBuffer,actions.QueryAction)) = {
    obs=qa._1
    val r=query(qa._2)
    obs=null
    r
  }
}

