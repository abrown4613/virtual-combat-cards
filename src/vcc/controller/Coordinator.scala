//$Id$
package vcc.controller

import scala.actors.Actor

package actions {
  case class SetCoordinator(coord:Coordinator)
  case class AddObserver(obs:Actor)
}

/**
 * This define the business logic behavior of the Tracker, it allows
 * you to add handlers for processing TransactionalActions and QueryActions.
 * This is designed to be subclassed for each game system. 
 */
abstract class TrackerController[C](val context:C) {
  
  private var handlers:List[TransactionalActionHandler[C]]=Nil
  private var querys:List[QueryActionHandler[C]]=Nil
  private var publishers:List[ChangePublisher[C]]=Nil
  
  /**
   * Add a TransactionalAction handler.
   */
  protected def addHandler(hndl:TransactionalActionHandler[C]) {
    handlers = handlers ::: List(hndl)
  }
  
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
    for(hndl<-handlers) {
      if(hndl.isDefinedAt(trans,msg))
        hndl(trans,msg)
    }
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

object Coordinator {

  def initialize[C](tc:TrackerController[C]):Coordinator = {
    val tracker=new Tracker(tc)
    val loader=new EntityLoader();
    
    new Coordinator(tracker,loader)
  }
}

class Coordinator(val tracker:Actor, val loader:Actor) {
  def start() {
    tracker.start
    loader.start
    tracker ! actions.SetCoordinator(this)
    loader ! actions.SetCoordinator(this)
  }
  
  def addObserver(obs:Actor) {
    tracker ! actions.AddObserver(obs)
  }
}
