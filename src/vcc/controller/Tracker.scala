//$Id$
package vcc.controller

import scala.actors.Actor
import scala.actors.Actor.loop

import vcc.controller.transaction._

/**
 * Tracker actor handles the core logic for the event dispatch loop. It controls
 * transactions (start, end and clearing the log), undo/redo, and observer registration.
 * It will dispatch actions and query to the controller handlers, and will gather return
 * data to be passed on to the observer.
 * @param controller Action and query logic controller
 */
class Tracker[C](controller:TrackerController[C]) extends Actor with TransactionChangePublisher {
  
  //private var uia:Actor=null
  private var observers:List[Actor]=Nil
  
  private var _coord:Coordinator =null

  private val _tlog= new TransactionLog[actions.TransactionalAction]()
  
  class ComposedAction(val name:String) extends actions.TransactionalAction {
    private var acts:List[actions.TransactionalAction] =Nil
    def add(act:actions.TransactionalAction) {
      acts=act::acts
    }
    def description():String= name
    lazy val transaction=new Transaction()
  }
  
  private var _composedAction: ComposedAction = null
  
  def startTransaction() = if(_composedAction!=null) _composedAction.transaction else new Transaction()
  
  def closeTransaction(action:actions.TransactionalAction, trans:Transaction) {
    if(_composedAction==null || (_composedAction eq action)) {
      //Need to close composed transcation or a simple transaction
      trans.commit(this) 
      if(!trans.isEmpty) {
        _tlog.store(action,trans)
        println("TLOG["+ _tlog.length +"] Added transaction: "+ _tlog.pastActions.head.description)
      }
    } else if(_composedAction!=null ) {
      //save transaction into the composed action
      _composedAction.add(action)
    }
  }
  
  private def sendToObservers(mbuf:TrackerResponseBuffer) {
    for(obs<-observers) {
      for(msg<-mbuf.messages) obs ! msg
    }
  }
  
  /**
   * Publish changes to the observers
   */
  def publishChange(changes:Seq[ChangeNotification]) {
    val pbuf=new TrackerResponseBuffer()
    controller.publish(changes,pbuf)
    sendToObservers(pbuf)
  }
  
  def act()={
    loop {
      react {
        case actions.AddObserver(obs) => 
          observers=obs::observers
        case actions.SetCoordinator(coord) => 
          this._coord=coord
          
        case ta:actions.TransactionalAction => 
          val trans=startTransaction()
          try {
        	controller.dispatch(trans,ta)
        	closeTransaction(ta,trans)
          } catch {
            case e => 
              println("An exception occured while processing: "+ ta)
              e.printStackTrace(System.out)
              println("Rolling back transaction")
              trans.cancel()
          }
          
        case actions.StartTransaction(tname) =>
          if(_composedAction==null)
            _composedAction=new ComposedAction(tname)
          else
            throw new Exception("Cant nest transaction")
        case actions.EndTransaction(tname) => 
          if(_composedAction!=null) {
            if(_composedAction.name==tname) { 
              closeTransaction(_composedAction,_composedAction.transaction)
              _composedAction=null
            } else throw new Exception("Tranction name mismatch, expected"+_composedAction.name+" found "+tname)
          } else throw new Exception("Not in compound transaction")
        case qa:actions.QueryAction =>
          val buf=new TrackerResponseBuffer()
          controller.processQuery(qa,buf)
          if(buf.replyMessage!=null) reply(buf.replyMessage)
          sendToObservers(buf)
        case actions.Undo() =>
          try {
            _tlog.rollback(this)
          } catch { case s:TransactionLogOutOfBounds => }
        case actions.Redo() =>
          try {
            _tlog.rollforward(this)
          } catch { case s:TransactionLogOutOfBounds => }
        case actions.ClearTransactionLog() =>
          _tlog.clear
          
        case s=>println("Error: Tracker receive:"+s)
      }
    }
  }
}