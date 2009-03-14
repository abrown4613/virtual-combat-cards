//$Id$
package vcc.dnd4e.controller

import vcc.controller.actions.TransactionalAction
import vcc.controller.{ChangePublisher,TransactionalProcessor}

import vcc.dnd4e.model._
import vcc.dnd4e.controller.request._

/**
 * Handles EffectList related actions.
 */
trait TrackerEffectHandler {
  this:TransactionalProcessor[TrackerContext] =>

  addHandler {
    case AddEffect(context.InMap(c),effect) =>
      c.effects = c.effects.add(effect)
    case CancelEffect(context.InMap(c),pos) =>
      c.effects = c.effects.delete(pos)
    case UpdateEffect(context.InMap(c),pos,newcond) =>
      c.effects = c.effects.update(pos,newcond)
    case InternalInitiativeAction(c,InitiativeTracker.actions.StartRound) =>
      context.allCombatant.map(comb=>{comb.effects=comb.effects.startRound(c.id)})
    case InternalInitiativeAction(c,InitiativeTracker.actions.EndRound) =>
      context.allCombatant.foreach(comb=>{comb.effects=comb.effects.endRound(c.id)})
    case ApplyRest(dontcare)=>
      context.allCombatant.map(comb=>{comb.effects=comb.effects.applyRest()})
    case SustainEffect(context.InMap(c),pos) =>
      c.effects = c.effects.sustain(pos)
    case InternalInitiativeAction(c,InitiativeTracker.actions.Delay) =>
      context.allCombatant.foreach(comb=>{
        val ally= (CombatantType.isCharacter(comb.ctype)==CombatantType.isCharacter(c.ctype))
        comb.effects=comb.effects.processDelay(ally,c.id)
      })
  }                                                                               
}

class TrackerEffectPublisher(context:TrackerContext) extends ChangePublisher[TrackerContext] {
  def publish(context:TrackerContext, changes:Seq[vcc.controller.transaction.ChangeNotification],buffer:vcc.controller.TrackerResponseBuffer) {
    changes.foreach {
      case CombatantUpdate(c,el:EffectList)=>
        buffer ! response.UpdateEffects(c,el.effects)
      case _ =>
    }	
  }
}