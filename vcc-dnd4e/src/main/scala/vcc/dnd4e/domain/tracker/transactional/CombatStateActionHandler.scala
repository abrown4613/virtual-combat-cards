/**
 * Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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

import vcc.dnd4e.domain.tracker.common.Command._
import vcc.controller.IllegalActionException

trait CombatStateActionHandler {
  this: CombatController =>

  addHandler {
    case StartCombat() =>
      if (!context.isCombatStarted) {
        if (rules.hasActingCombatant(context)) {
          context.metaData.startCombat()
          context.order.startCombat()
        }
        else throw new IllegalActionException("Must have at least on combatant in order.")
      } else throw new IllegalActionException("Combat already started.")

    case EndCombat() =>
      if (context.isCombatStarted) {
        context.metaData.endCombat()
        context.order.clearOrder()
      } else throw new IllegalActionException("Combat already ended.")

    case AddCombatants(combatants) =>
      combatants.map(cre => context.roster.addCombatant(cre.cid, cre.alias, cre.entity))

    case SetInitiative(id) =>
      for (iDef <- id) {
        if (rules.canCombatantRollInitiative(context, iDef.combId))
          context.order.setInitiative(iDef)
        else
          throw new IllegalActionException("Combatant " + iDef.combId + " cant roll initiative.")
      }

    case SetCombatComment(comment) =>
      context.metaData.comment = comment
  }
}