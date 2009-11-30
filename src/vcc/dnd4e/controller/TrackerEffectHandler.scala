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

import vcc.controller.message.TransactionalAction
import vcc.controller.{TransactionalProcessor}

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
