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
package vcc.dnd4e.controller

import vcc.controller.TransactionalProcessor
import vcc.controller.actions.TransactionalAction
import vcc.dnd4e.model._
import vcc.dnd4e.controller._
import vcc.controller.ChangePublisher
import InitiativeTracker.actions._
import request.InternalInitiativeAction

trait InitiativeActionHandler extends TransactionalProcessor[TrackerContext]{
  this: TransactionalProcessor[TrackerContext] =>

  def cmap=context.map
  val sequence=context.sequence
  
  /**
   * We need to rewrite composed initiative operation to their internal versions.
   */
  override def rewriteEnqueue(msg:TransactionalAction) {
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
      case request.Ready(context.InMap(c)) => msgQueue.enqueue(InternalInitiativeAction(c,Ready),InternalInitiativeAction(c,EndRound))
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
    case request.EndCombat() => {
      context.allCombatant.foreach(c=>{
        c.it.value=InitiativeTracker(0,InitiativeState.Reserve)
        c.health=c.health.setTemporaryHitPoints(0,true)
        sequence.add(c.id)
      })
    }
    
    case request.MoveBefore(context.InMap(who),context.InMap(before)) =>
      if(who.it.value.state != InitiativeState.Acting && before.it.value.state != InitiativeState.Acting ) {
        sequence.moveBefore(who.id,before.id)
        advanceDead()
      }
      
    case request.InternalInitiativeAction(cmb,action) =>
      import InitiativeTracker.actions._
      import request.InternalInitiativeAction
      
      
      val firstp=this.context.map(sequence.sequence.head).id==cmb.id
      var itt=cmb.it.value
      if(itt.canTransform(firstp,action)) {
    	cmb.it.value=itt.transform(firstp,action)
    	action match {
    	  case Delay => 
    	    sequence.moveDown(cmb.id)
    	    advanceDead()
    	  case MoveUp => sequence.moveUp(cmb.id)
    	  case ExecuteReady => sequence.moveDown(cmb.id)
    	  case EndRound if(itt.state!=InitiativeState.Delaying)=>
    	    sequence.moveDown(cmb.id)
    	    advanceDead()
    	  case _ =>
    	}
      } else {
        throw new Exception("Error: Cant process "+(firstp,action)+ " on state "+itt +" of "+cmb.id.name)
      }
  }
  def advanceDead() {
	//Close round and out advance dead
	val next=context.map(context.sequence.sequence.head)
	if(next.health.status == HealthTracker.Status.Dead) {
	  // Need to check if the dead combatant is delay, and end its round accordingly
	  if(next.it.value.state == InitiativeState.Delaying) {
		msgQueue.enqueue(InternalInitiativeAction(next,EndRound),InternalInitiativeAction(next,StartRound),InternalInitiativeAction(next,EndRound))
	  } else {
		msgQueue.enqueue(InternalInitiativeAction(next,StartRound),InternalInitiativeAction(next,EndRound))
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