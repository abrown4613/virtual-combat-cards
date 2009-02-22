//$Id$
package vcc.dnd4e.controller

import vcc.controller.actions.TransactionalAction
import vcc.controller.{ChangePublisher,TransactionalActionHandler}

import vcc.dnd4e.model._
import vcc.dnd4e.controller.request._

/**
 * Handles EffectList related actions
 */
class TrackerEffectHandler(context:TrackerContext) extends TransactionalActionHandler(context) {
  val handler:PartialFunction[TransactionalAction,Unit] = {
    case AddEffect(context.InMap(c),effect) =>
      c.effects = c.effects.add(effect)
    case CancelEffect(context.InMap(c),pos) =>
      c.effects = c.effects.delete(pos)
    case StartRound(context.InMap(c)) =>
      context.allCombatant.map(comb=>{comb.effects=comb.effects.startRound(c.id)})
    case EndRound(context.InMap(c)) =>
      context.allCombatant.map(comb=>{comb.effects=comb.effects.endRound(c.id)})
    case ApplyRest(dontcare)=>
      context.allCombatant.map(comb=>{comb.effects=comb.effects.applyRest()})
    case SustainEffect(context.InMap(c),pos) =>
      c.effects = c.effects.sustain(pos)
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