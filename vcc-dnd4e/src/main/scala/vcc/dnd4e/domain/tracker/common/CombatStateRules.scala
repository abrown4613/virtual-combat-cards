/**
 *  Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.common


class CombatStateRules {

  /**
   * Inform if all the Combatants with and InitiativeOrderID are not dead.
   */
  def areAllCombatantInOrderDead(combatState: CombatStateView): Boolean = {
    !combatState.allInitiativeOrderIDs.exists(x => combatState.combatantFromID(x.combId).healthTracker.status != HealthTracker.Status.Dead)
  }

  def canCombatantRollInitiative(combatState: CombatStateView, combID: CombatantID): Boolean = {
    if (combatState.isCombatStarted) {
      !combatState.allInitiativeOrderIDs.exists(ioi => ioi.combId == combID)
    } else true
  }
}