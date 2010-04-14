//$Id$

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

package vcc.dnd4e.view.tabular

import vcc.dnd4e.model._
import common.HealthTracker

object CombatantStateProjection extends vcc.util.swing.TableModelRowProjection[CombatantState] {
  override val columns = List[(String, java.lang.Class[_])](
    ("ID", classOf[java.lang.String]),
    ("Name", classOf[String]),
    ("Health", classOf[String]),
    ("Status", classOf[String]),
    ("Turn #", classOf[Integer]),
    ("Sequence", classOf[String])
    );
  def apply(col: Int, comb: CombatantState): java.lang.Object = {
    col match {
      case 0 => comb.id.name
      case 1 => (if (comb.alias != null) "[" + comb.alias + "] " else "") + comb.entity.name
      case 2 => comb.health.currentHP + " / " + comb.health.base.totalHP + (if (comb.health.temporaryHP > 0) " +" + comb.health.temporaryHP else "")
      case 3 => comb.health.status + (if (comb.health.status == HealthTracker.Status.Dying) ("(" + comb.health.deathStrikes + "/3)") else "!!!".substring(0, comb.health.deathStrikes))
      case 4 => int2Integer(comb.init.round)
      case 5 => comb.init.state
    }
  }

  val setter = null
}
