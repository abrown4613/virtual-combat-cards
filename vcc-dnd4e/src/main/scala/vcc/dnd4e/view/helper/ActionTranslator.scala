/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.view.helper

import vcc.controller.message.TransactionalAction
import vcc.dnd4e.domain.tracker.common.Command._
import vcc.dnd4e.domain.tracker.common.{CombatStateView}
import vcc.dnd4e.tracker.common.InitiativeTracker

object ActionTranslator {
  private def formatIfNotNull(s: String, fmt: String) = if (s != null) s.formatted(fmt) else ""

  def fullActionMessage(state: CombatStateView, action: TransactionalAction): String = {
    action match {
      case InternalInitiativeAction(ioi, InitiativeTracker.action.StartRound) =>
        val cmb = state.combatantViewFromID(ioi.combId)
        "Start round of " + cmb.definition.entity.name + formatIfNotNull(cmb.definition.alias, " - %s") + " [" + ioi.toLabelString + "]"
      case InternalInitiativeAction(ioi, InitiativeTracker.action.EndRound) =>
        val cmb = state.combatantViewFromID(ioi.combId)
        "End round of " + cmb.definition.entity.name + formatIfNotNull(cmb.definition.alias, " - %s") + " [" + ioi.toLabelString + "]"
      case _ => action.description
    }
  }
}