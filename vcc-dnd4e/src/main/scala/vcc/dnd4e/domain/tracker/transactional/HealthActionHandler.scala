/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.transactional

import vcc.dnd4e.domain.tracker.common._
import Command._

trait HealthActionHandler {
  this: AbstractCombatController =>

  addHandler {
    case ApplyDamage(context.combatantFromID(comb), damage) =>
      val newHealth = comb.health.applyDamage(damage)
      comb.health = newHealth
      if (newHealth.status() == HealthTracker.Status.Dead && rules.areAllCombatantInOrderDead(context)) {
        context.order.clearOrder()
        context.metaData.endCombat()
      }

    case HealDamage(context.combatantFromID(comb), healing) =>
      comb.health = comb.health.heal(healing)

    case SetTemporaryHP(context.combatantFromID(comb), temporaryHP) =>
      comb.health = comb.health.setTemporaryHitPoints(temporaryHP, false)

    case FailDeathSave(context.combatantFromID(comb)) =>
      comb.health = comb.health.failDeathSave()

    case RevertDeath(context.combatantFromID(comb)) =>
      comb.health = comb.health.raiseFromDead()

    case ApplyRest(extended) =>
      for (comb <- context.roster.allCombatantIDs.map(id => context.roster.combatant(id))) {
        comb.health = comb.health.rest(extended)
      }

    case SetComment(context.combatantFromID(comb), text) =>
      comb.comment = text

  }
}