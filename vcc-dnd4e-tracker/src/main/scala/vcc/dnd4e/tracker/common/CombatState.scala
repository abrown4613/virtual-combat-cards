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
package vcc.dnd4e.tracker.common

import vcc.dnd4e.tracker.transition.CombatTransition
import vcc.dnd4e.tracker.StateLensFactory

case class CombatState(roster: Roster[Combatant], order: InitiativeOrder, comment: Option[String]) {
  def endCombat(): CombatState = {
    this.copy(order = order.endCombat())
  }

  def startCombat(): CombatState = {
    copy(order = order.startCombat())
  }

  def isCombatStarted = order.nextUp.isDefined

  val rules = CombatState.rules

  def transitionWith(trans: List[CombatTransition]): CombatState = {
    trans.foldLeft(this)((s, x) => x.transition(StateLensFactory, s))
  }

}

object CombatState {

  val rules = new Rules()

  class Rules() {
    /**
     * Inform if all the Combatants with and InitiativeOrderID are not dead.
     */
    def areAllCombatantInOrderDead(combatState: CombatState): Boolean = {
      !combatState.order.order.exists(x => combatState.roster.entries(x.combId).health.status != HealthTracker.Status.Dead)
    }

    def canCombatantRollInitiative(combatState: CombatState, combID: CombatantID): Boolean = {
      if (combatState.isCombatStarted) {
        !combatState.order.order.exists(ioi => ioi.combId == combID)
      } else true
    }

    def hasActingCombatant(combatState: CombatState): Boolean = {
      combatState.order.order.length > 0
    }

    /**
     * Can moveBefore if who is not acting and is different than whom
     * @param state The current combatState
     * @param who Who will move
     * @param whom In front of whom who will move
     */
    def canMoveBefore(combatState: CombatState, who: InitiativeOrderID, whom: InitiativeOrderID): Boolean = {
      (who != whom) && combatState.isCombatStarted && combatState.order.tracker(who).state != InitiativeTracker.state.Acting
    }

    /**
     * Determines in a given action can be applied to an InitiativeTracker. Not that this does not cover compound
     * operations like the Delay which internally is broken into Start and Delay.
     */
    def canInitiativeOrderPerform(combatState: CombatState, io: InitiativeOrderID, action: InitiativeTracker.action.Value): Boolean = {
      if (!combatState.isCombatStarted || !combatState.order.nextUp.isDefined) false
      else {
        val first = combatState.order.tracker(combatState.order.nextUp.get)
        val test = combatState.order.tracker(io)
        test.canTransform(first, action)
      }
    }

    def areAllied(combatState: CombatState, combA: CombatantID, combB: CombatantID): Boolean = {
      val a = combatState.roster.entries(combA).definition.entity.ctype
      val b = combatState.roster.entries(combB).definition.entity.ctype
      (a == CombatantType.Character && b == CombatantType.Character) || (a != CombatantType.Character && b != CombatantType.Character)
    }

  }

  def empty = CombatState(Roster(Combatant.RosterFactory, Map()), InitiativeOrder.empty(), None)
}
