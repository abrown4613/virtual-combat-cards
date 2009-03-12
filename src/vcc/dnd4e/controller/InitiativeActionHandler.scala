//$Id$
package vcc.dnd4e.controller

import vcc.controller.TransactionalProcessor
import vcc.controller.actions.TransactionalAction
import vcc.dnd4e.model._
import vcc.dnd4e.controller._
import vcc.controller.ChangePublisher

trait InitiativeActionHandler extends TransactionalProcessor[TrackerContext]{
  this: TransactionalProcessor[TrackerContext] =>

  val cmap=context.map
  val sequence=context.sequence
  
  override def rewriteEnqueue(msg:TransactionalAction) {
    import InitiativeTracker.actions._
    import request.InternalInitiativeAction
    msg match {
      case request.Delay(context.InMap(c)) => 
      	msgQueue.enqueue(InternalInitiativeAction(c,StartRound),InternalInitiativeAction(c,Delay))
      case request.MoveUp(context.InMap(c)) =>
        c.it.value match {
          case InitiativeTracker(round,InitiativeState.Delaying) => 
            msgQueue.enqueue(InternalInitiativeAction(c,MoveUp))
          case _ =>  
            msgQueue.enqueue(InternalInitiativeAction(c,MoveUp),InternalInitiativeAction(c,StartRound))
        }
      case request.StartRound(context.InMap(c))=> msgQueue.enqueue(InternalInitiativeAction(c,StartRound))
      case request.EndRound(context.InMap(c))=> msgQueue.enqueue(InternalInitiativeAction(c,EndRound))
      case request.Ready(context.InMap(c)) => msgQueue.enqueue(InternalInitiativeAction(c,Ready))
      case request.ExecuteReady(context.InMap(c)) => msgQueue.enqueue(InternalInitiativeAction(c,ExecuteReady))
      case _ => super.rewriteEnqueue(msg)
    }
  }
  
  addHandler {
    case request.StartCombat(seq) =>
      for(x<-seq) {
        if(cmap.contains(x)) {
          var c=cmap(x)
          sequence.moveDown(c.id)
          c.it.value=InitiativeTracker(0,InitiativeState.Waiting)
        }
      }
    case request.InternalInitiativeAction(cmb,action) =>
      import InitiativeTracker.actions._
      import request.InternalInitiativeAction
      
      val firstp=this.context.map(sequence.sequence.head).id==cmb.id
      var itt=cmb.it.value
      if(itt.canTransform(firstp,action)) {
    	cmb.it.value=itt.transform(firstp,action)
    	action match {
    	  case Delay => sequence.moveDown(cmb.id)
    	  case Ready => sequence.moveDown(cmb.id)
    	  case MoveUp => sequence.moveUp(cmb.id)
    	  case ExecuteReady => sequence.moveDown(cmb.id)
    	  case EndRound => //Close round and out advance dead
    	    sequence.moveDown(cmb.id)
    	    val next=context.map(context.sequence.sequence.head)
    	    if(next.health.status == HealthTracker.Status.Dead) 
    	      msgQueue.enqueue(InternalInitiativeAction(next,StartRound),InternalInitiativeAction(next,EndRound))
    	  case _ =>
    	}
      }
  }
}

class InitiativeChangePublisher(context:TrackerContext) extends ChangePublisher[TrackerContext] {
  def publish(context:TrackerContext, changes:Seq[vcc.controller.transaction.ChangeNotification],buffer:vcc.controller.TrackerResponseBuffer) {
    changes.foreach {
      case s:vcc.dnd4e.view.actor.SetSequence => buffer ! s
      case CombatantUpdate(comb, s:InitiativeTracker) => buffer ! vcc.dnd4e.view.actor.SetInitiative(comb,s)
      case _ =>
    }	
  }
}