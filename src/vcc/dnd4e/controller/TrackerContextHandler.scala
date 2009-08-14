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

import vcc.controller.{TransactionalProcessor,ChangePublisher}
import vcc.controller.transaction._
import vcc.controller.actions.TransactionalAction
import vcc.dnd4e.model._
import vcc.dnd4e.controller._

import vcc.model.Registry
import vcc.model.datastore.EntityStore

trait TrackerContextHandler {
  this:TransactionalProcessor[TrackerContext] =>

  import context._
 
  addHandler {
    case request.AddCombatant(store,member)=>
      var id:Symbol=if(member.id==null) context.idgen.first() else {
        var s=member.id
        if(context.idgen.contains(s)) context.idgen.removeFromPool(s) // To make sure we don't get doubles
        s
      }
      val repository = Registry.get[EntityStore](store).get
      if(repository == null) throw new Exception("Cant find store "+store)
      val ent = repository.load(member.eid).asInstanceOf[CombatantEntity]
      if(ent == null) throw new Exception("Can't find entity "+member.eid+ " in EntityStore")
      var nc=new TrackerCombatant(id,member.alias, ent.name.value.get,ent.hp.value.get,ent.initiative.value.get,ent.combatantType,ent)
      nc.defense=new DefenseBlock(ent.ac.value.get, ent.fortitude.value.get, ent.reflex.value.get, ent.will.value.get)
      if(context.map.contains(nc.id)) {
        // It's an old combatant salvage old heath and Initiative
        val oc=context.map(nc.id)
        nc.health = oc.health.replaceHealthDefinition(nc.health.base)
        nc.it =oc.it
        nc.effects=oc.effects
      } else {
        context.sequence add id
      }
      map = map + (id -> nc)

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
      buffer ! vcc.dnd4e.view.actor.Combatant(vcc.dnd4e.view.ViewCombatant(x.id,x.alias,x.name,x.hp,x.init,x.defense,x.entity))
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