/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.tracker.common

class CombatStateRules {

  /**
   * Inform if all the Combatants with and InitiativeOrderID are not dead.
   */
  def areAllCombatantInOrderDead(combatState: CombatStateView): Boolean = {
    !combatState.getInitiativeOrder.exists(x => combatState.combatantViewFromID(x.combId).healthTracker.status != HealthStatus.Dead)
  }

  def canCombatantRollInitiative(combatState: CombatStateView, combID: CombatantID): Boolean = {
    if (combatState.isCombatStarted) {
      !combatState.getInitiativeOrder.exists(ioi => ioi.combId == combID)
    } else true
  }

  def hasActingCombatant(combatState: CombatStateView): Boolean = {
    combatState.getInitiativeOrder.length > 0
  }

  /**
   * Can moveBefore if who is not acting and is different than whom
   * @param combatState The current combatState
   * @param who Who will move
   * @param whom In front of whom who will move
   */
  def canMoveBefore(combatState: CombatStateView, who: InitiativeOrderID, whom: InitiativeOrderID): Boolean = {
    (who != whom) && combatState.isCombatStarted && combatState.initiativeTrackerFromID(who).state != InitiativeState.Acting
  }

  /**
   * Determines in a given action can be applied to an InitiativeTracker. Not that this does not cover compound
   * operations like the Delay which internally is broken into Start and Delay.
   */
  def canInitiativeOrderPerform(combatState: CombatStateView, io: InitiativeOrderID, action: InitiativeAction.Value): Boolean = {
    if (!combatState.isCombatStarted || !combatState.nextUp.isDefined) false
    else {
      val first = combatState.initiativeTrackerFromID(combatState.nextUp.get)
      val test = combatState.initiativeTrackerFromID(io)
      test.canTransform(first, action)
    }
  }

  def areAllied(combatState: CombatStateView, combA: CombatantID, combB: CombatantID): Boolean = {
    val a = combatState.combatantViewFromID(combA).combatantType
    val b = combatState.combatantViewFromID(combB).combatantType
    (a == CombatantType.Character && b == CombatantType.Character) || (a != CombatantType.Character && b != CombatantType.Character)
  }
}