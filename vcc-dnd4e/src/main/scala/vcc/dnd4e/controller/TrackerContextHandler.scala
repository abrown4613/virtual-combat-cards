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

import vcc.controller.{TransactionalProcessor}
import vcc.controller.transaction._
import vcc.controller.message.TransactionalAction
import vcc.dnd4e.model._
import common.CombatantType
import vcc.dnd4e.controller._
import vcc.dnd4e.controller.request._

import vcc.model.Registry

trait TrackerContextHandler {
  this: TransactionalProcessor[TrackerContext] =>

  import context._

  private def addCombatant(member: CombatantDefinition) {
    var id: Symbol = if (member.id == null) context.idgen.first() else {
      var s = member.id
      if (context.idgen.contains(s)) context.idgen.removeFromPool(s) // To make sure we don't get doubles
      s
    }
    val ent = CombatantRepository.getEntity(member.ceid)
    if (ent == null) throw new Exception("Can't find entity " + member.ceid + " in EntityStore")
    var nc = new TrackerCombatant(id, member.alias, ent.name, ent.healthDef, ent.initiative, ent.ctype, ent)
    if (context.map.contains(nc.id)) {
      // It's an old combatant salvage old heath and Initiative
      val oc = context.map(nc.id)
      nc.health = oc.health.replaceHealthDefinition(nc.health.base)
      nc.it.value = oc.it.value
      nc.effects = oc.effects
    } else {
      context.sequence add id
    }
    map = map + (id -> nc)
  }

  addHandler {
    case request.AddCombatants(combs) =>
      for (comb <- combs) addCombatant(comb)

    case request.ClearCombatants(all) =>
      var current = map.keySet.toList
      if (all) {
        map = Map.empty[Symbol, TrackerCombatant]
      } else {
        map = map.filter(p => p._2.health.base.ctype == CombatantType.Character)
      }
      var removed = current -- map.keySet.toList
      for (x <- removed) {
        idgen.returnToPool(x)
      }
      sequence.removeFromSequence(removed)

    case request.ApplyRest(extended) => {
      for (p <- map) {
        var c = p._2
        c.health = c.health.rest(extended)
      }
    }

    // HEALTH Tracking
    case ApplyDamage(InMap(c), amnt) =>
      c.health = c.health.applyDamage(amnt)
    case HealDamage(InMap(c), amnt) =>
      c.health = c.health.heal(amnt)
    case SetTemporaryHP(InMap(c), amnt) =>
      c.health = c.health.setTemporaryHitPoints(amnt, false)
    case FailDeathSave(InMap(c)) =>
      c.health = c.health.failDeathSave()
    case Undie(InMap(c)) => c.health = c.health.raiseFromDead

    case SetComment(InMap(c), text) =>
      c.info = text

  }
}