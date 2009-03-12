//$Id$
package vcc.dnd4e.controller

import vcc.controller.{TransactionalProcessor,ChangePublisher}
import vcc.controller.transaction._
import vcc.controller.actions.TransactionalAction
import vcc.dnd4e.model._
import vcc.dnd4e.controller._

trait TrackerContextHandler {
  this:TransactionalProcessor[TrackerContext] =>

  import context._
 
  addHandler {
    case request.AddCombatant(template)=>
      var id:Symbol=if(template.id==null) context.idgen.first() else {
        var s=Symbol(template.id)
        if(context.idgen.contains(s)) context.idgen.removeFromPool(s) // To make sure we don't get doubles
        s
      }
      var nc=new TrackerCombatant(id,template.name,template.hp,template.init,template.ctype)
      nc.defense=template.defense
      if(context.map.contains(nc.id)) {
        // It's an old combatant salvage old heath and Initiative
        nc.health = context.map(nc.id).health
        nc.it =context.map(nc.id).it
      } else {
        context.sequence add id
      }
      map = map + (id -> nc)
    //MOVE OUT
    case request.StartCombat(seq) =>
      for(x<-seq) {
        if(map.contains(x)) {
          var c=map(x)
          sequence.moveDown(c.id)
          c.it.value=InitiativeTracker(0,InitiativeState.Waiting)
        }
      } 
    //END MOVE OUT
    case request.ClearCombatants(all) =>
      var current=map.keySet.toList
      if(all) {
        map=Map.empty[Symbol,TrackerCombatant]
      } else {
        map=map.filter(p=>p._2.health.base.ctype==CombatantType.Character)
      }
      var removed=current -- map.keySet.toList
      for(x <- removed) {
        idgen.returnToPool(x)
      }
      sequence.removeFromSequence(removed)
      
    //MOVE OUT
    case request.EndCombat() => {
      for(p<-map) {
        var c=p._2
        c.it.value=InitiativeTracker(0,InitiativeState.Reserve)
        c.health=c.health.setTemporaryHitPoints(0,true)
        sequence.add(c.id)
      }
    }
    //END MOVE OUT
    
    case request.ApplyRest(extended) => {
      for(p<-map) {
        var c=p._2
        c.health=c.health.rest(extended)
      }
    }
    
    // HEALTH Tracking
    case vcc.dnd4e.controller.actions.ApplyDamage(InMap(c),amnt) =>
      c.health=c.health.applyDamage(amnt)
    case vcc.dnd4e.controller.actions.HealDamage(InMap(c),amnt) =>
      c.health=c.health.heal(amnt)
    case vcc.dnd4e.controller.actions.SetTemporaryHP(InMap(c),amnt) =>
      c.health=c.health.setTemporaryHitPoints(amnt,false)
    case vcc.dnd4e.controller.actions.FailDeathSave(InMap(c)) =>
      c.health=c.health.failDeathSave()
    case vcc.dnd4e.controller.actions.Undie(InMap(c)) => c.health=c.health.raiseFromDead
      
    case vcc.dnd4e.controller.actions.SetComment(InMap(c),text)=>
      c.info=text
      
    //MOVE OUT
      // INITIATIVE TRACKING  
    case request.MoveUp(InMap(c)) => 
      this.changeSequence(c,InitiativeTracker.actions.MoveUp)
    case request.StartRound(InMap(c)) =>
      this.changeSequence(c,InitiativeTracker.actions.StartRound)
    case request.EndRound(InMap(c)) =>
      this.changeSequence(c,InitiativeTracker.actions.EndRound)
    case request.Delay(InMap(c)) =>
      this.changeSequence(c,InitiativeTracker.actions.Delay)
    case request.Ready(InMap(c)) => 
      this.changeSequence(c,InitiativeTracker.actions.Ready)
    case request.ExecuteReady(InMap(c)) =>
      this.changeSequence(c,InitiativeTracker.actions.ExecuteReady)
    //END MOVE OUT
  }
  
  def changeSequence(cmb:TrackerCombatant,action:InitiativeTracker.actions.Value)(implicit trans:Transaction) {
    var itt=cmb.it.value
    var firstp=map(sequence.sequence.head).id==cmb.id
    if(itt.canTransform(firstp,action)) {
      cmb.it.value=itt.transform(firstp,action)
      action match {
        case InitiativeTracker.actions.Delay => 
          sequence.moveDown(cmb.id)
          advanceToNext
        case InitiativeTracker.actions.ExecuteReady => 
          sequence.moveDown(cmb.id)
        case InitiativeTracker.actions.EndRound =>
          // When delaying is up, end turn is end of previous
          if(cmb.it.value.state!=InitiativeState.Delaying) { 
            sequence.rotate
            advanceToNext()
          }
        case InitiativeTracker.actions.Ready => 
          sequence.moveDown(cmb.id)
          advanceToNext
        case InitiativeTracker.actions.MoveUp => 
          sequence.moveUp(cmb.id)
        case _ =>
      }
    }
  }

  private def advanceToNext()(implicit trans:Transaction) {
    // Auto advance dead guys
    while(map(sequence.sequence.head).health.status==HealthTracker.Status.Dead) {
      var dcmb=map(sequence.sequence.head)
      var dit=dcmb.it
      dit.value=dit.value.transform(true,InitiativeTracker.actions.StartRound)
      dit.value=dit.value.transform(true,InitiativeTracker.actions.EndRound)
      dcmb.it=dit
      sequence.rotate
    }
  }
}
  
class DefaultChangePublisher extends ChangePublisher[TrackerContext] {
  /**
   * Publish changes to the observers
   */
  def publish(context:TrackerContext, changes:Seq[vcc.controller.transaction.ChangeNotification],buffer:vcc.controller.TrackerResponseBuffer) {
    changes.foreach {
      //TODO: Move out
      case CombatantUpdate(comb, s:InitiativeTracker) => buffer ! vcc.dnd4e.view.actor.SetInitiative(comb,s)

      case RosterUpdate(map) => enumerate(context,map,buffer)
      case CombatantUpdate(comb, h:HealthTracker) => buffer ! vcc.dnd4e.view.actor.SetHealth(comb,h)
      case CombatantUpdate(comb, info:String) => buffer  ! vcc.dnd4e.view.actor.SetInformation(comb,info)
      //TODO: Move out
      case s:vcc.dnd4e.view.actor.SetSequence => buffer ! s
      case _ => //Ignore to avoid exception
    }
  }

  private def enumerate(context:TrackerContext,map:Map[Symbol,TrackerCombatant],buffer:vcc.controller.TrackerResponseBuffer) {
    TrackerContextEnumerator.enumerate(context,buffer)
    // Return ids to generator, 
    // TODO: This is not a publishing aspect shoudl be somewhere else? 
    for(id<-context.idgen.leasedSymbols) {
      if(!context.map.contains(id)) context.idgen.returnToPool(id)
    }
    for(id<-map.map(_._1))
        if(context.idgen.contains(id)) context.idgen.removeFromPool(id)
  }
}

object TrackerContextEnumerator {
  def enumerate(context:TrackerContext,buffer:vcc.controller.TrackerResponseBuffer) { 
    buffer ! vcc.dnd4e.view.actor.ClearSequence()
    for(x<-context.map.map(_._2)) { 
      buffer ! vcc.dnd4e.view.actor.Combatant(vcc.dnd4e.view.ViewCombatant(x.id,x.name,x.hp,x.init,x.defense))
      buffer ! vcc.dnd4e.view.actor.SetHealth(x.id,x.health)
      buffer ! vcc.dnd4e.view.actor.SetInitiative(x.id,x.it.value)
      buffer ! vcc.dnd4e.view.actor.SetInformation(x.id,x.info)
    }
    buffer ! vcc.dnd4e.view.actor.SetSequence(context.sequence.sequence)
  }
}

import vcc.controller.QueryActionHandler
import vcc.controller.actions.QueryAction

class TrackerQueryHandler(context:TrackerContext) extends QueryActionHandler(context){
  val query:PartialFunction[QueryAction,Unit]= {
    case vcc.dnd4e.controller.actions.QueryCombatantMap(func) =>
      obs reply context.map.map(x=>func(x._2)).toList
    case request.Enumerate()=> 
      TrackerContextEnumerator.enumerate(context,obs)
  }		

}