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
package vcc.dnd4e.tracker.dispatcher

import vcc.dnd4e.tracker.common._

object CombatStateViewAdapterBuilder {

  def buildView(state: CombatState): CombatStateView = {
    new CombatStateViewAdapter(state)
  }

  private class CombatStateViewAdapter(combatState: CombatState) extends CombatStateView {
    private val combatantViewMap = combatState.roster.entries.map(e => (e._1, e._2))

    def combatantViewFromID(id: CombatantID): Combatant = combatantViewMap(id)

    def allCombatantIDs: List[CombatantID] = combatState.roster.allCombatantIDs

    def initiativeTrackerFromID(orderId: InitiativeOrderID): InitiativeTracker = combatState.order.tracker(orderId)

    def getInitiativeOrder: List[InitiativeOrderID] = combatState.order.sequence

    def isCombatStarted: Boolean = combatState.isCombatStarted

    def combatComment: String = combatState.comment.getOrElse(null)

    def nextUp: Option[InitiativeOrderID] = combatState.order.nextUp
  }
}