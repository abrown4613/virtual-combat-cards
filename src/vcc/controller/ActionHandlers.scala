//$Id$
package vcc.controller

import vcc.controller.transaction.Transaction
import vcc.controller.actions.TransactionalAction

/**
 * TransactionalProcessor is a container for a set of PartialFunctions that 
 * process an action. They are all called in sequence, and should be defined via
 * traits in order to access context and transaction fields.
 */
class TransactionalProcessor[C](val context:C) {
  
  protected val msgQueue = new scala.collection.mutable.Queue[TransactionalAction]
  
  /**
   * This is the transaction holder, should only be used internally
   */
  protected implicit var trans:Transaction=null
  
  /**
   * A list of handler PartialFunctions that should be added in traits.
   */
  private var handlers:List[PartialFunction[TransactionalAction,Unit]]=Nil
  
  def addHandler(handler:PartialFunction[TransactionalAction,Unit]) {
    handlers=handlers:::List(handler)
  }
 
  def rewriteEnqueue(action:TransactionalAction) {
    msgQueue.enqueue(action)
  }
  
  /**
   * Call internal handlers, but first set the transaction and then unset the transaction
   */
  def dispatch(transaction:Transaction,action:TransactionalAction):Unit = { 
    rewriteEnqueue(action)
    trans=transaction
    try {
      while(!msgQueue.isEmpty) {
    	val msg=msgQueue.dequeue
    	for(hndl<-handlers) {
    		if(hndl.isDefinedAt(msg)) hndl.apply(msg)
    	}
      }
    } catch {
      // We had an exception, flush message buffer to avoid leaving trash messages
      case e => 
        msgQueue.clear()
        throw e
    }
    trans=null
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

