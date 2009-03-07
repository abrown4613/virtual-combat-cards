//$Id$
package vcc.controller

/**
 * This define the business logic behavior of the Tracker, it allows
 * you to add handlers for processing TransactionalActions and QueryActions.
 * This is designed to be subclassed for each game system. 
 */
abstract class TrackerController[C](val context:C) {
  
  protected val processor:TransactionalProcessor[C]
  
  private var querys:List[QueryActionHandler[C]]=Nil
  private var publishers:List[ChangePublisher[C]]=Nil
  
  /**
   * Add a QueryAction handler.
   */
  protected def addQueryHandler(qhndl:QueryActionHandler[C]) {
    querys=querys:::List(qhndl)
  }
  
  /**
   * Add a publisher handler.
   */
  protected def addPublisher(pub:ChangePublisher[C]) {
    publishers=publishers:::List(pub)
  }
  
  /**
   * Process a TransactionalAction through all handlers in sequence.
   */
  def dispatch(trans:transaction.Transaction,msg:actions.TransactionalAction) {
    processor.dispatch(trans,msg)
  }
  
  /**
   * Process a QueryAction function through all query handlers 
   */
  def processQuery(query:actions.QueryAction,obs:TrackerResponseBuffer) {
    for(q <-querys if(q!=null && q.isDefinedAt(obs,query))) q(obs,query)
  }
  
  /**
   * Transform ChangeNotification into messages in the TrackerResponseBuffer.
   * The messages in the buffer will be sent by the Tracker object to all registered
   * observer.
   */
  def publish(changes:Seq[transaction.ChangeNotification],to:TrackerResponseBuffer) {
    publishers.foreach(_.publish(context,changes,to))
  }
}

