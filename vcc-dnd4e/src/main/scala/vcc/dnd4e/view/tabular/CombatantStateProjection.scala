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

package vcc.dnd4e.view.tabular

import vcc.dnd4e.domain.tracker.common.HealthTracker
import vcc.dnd4e.view.UnifiedCombatant

object CombatantStateProjection extends vcc.util.swing.TableModelRowProjection[UnifiedCombatant] {
  override val columns = List[(String, java.lang.Class[_])](
    ("ID", classOf[java.lang.String]),
    ("Name", classOf[String]),
    ("Health", classOf[String]),
    ("Status", classOf[String]),
    ("Turn #", classOf[Integer]),
    ("Sequence", classOf[String])
    );
  def apply(col: Int, comb: UnifiedCombatant): java.lang.Object = {
    col match {
      case 0 => comb.combId
      case 1 => (if (comb.combatant.definition.alias != null) "[" + comb.combatant.definition.alias + "] " else "") + comb.combatant.definition.entity.name
      case 2 =>
        val health = comb.combatant.healthTracker
        health.currentHP + " / " + health.base.totalHP + (if (health.temporaryHP > 0) " +" + health.temporaryHP else "")
      case 3 =>
        val health = comb.combatant.healthTracker
        health.status + (if (health.status == HealthTracker.Status.Dying) ("(" + health.deathStrikes + "/3)") else "!!!".substring(0, health.deathStrikes))
      case 4 => int2Integer(comb.initiative.round)
      case 5 => comb.initiative.state
    }
  }

  val setter = null
}
