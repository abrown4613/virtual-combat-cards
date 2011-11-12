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
package vcc.dnd4e.view.helper

import vcc.dnd4e.domain.tracker.snapshot.CombatState
import vcc.dnd4e.tracker.common.{CombatantID, InitiativeOrderID}

trait InitiativeOrderViewBuilder {
  def buildOrder(combatState: CombatState): Seq[InitiativeOrderID]
}

trait ReserveViewBuilder {
  def buildReserve(combatState: CombatState): Seq[CombatantID]
}

object SortedIDReserverViewBuilder extends ReserveViewBuilder {
  def buildReserve(combatState: CombatState): Seq[CombatantID] = {
    combatState.combatantsNotInOrder().toList.sortWith((a: CombatantID, b: CombatantID) => a.id < b.id)
  }
}

object DirectInitiativeOrderViewBuilder extends InitiativeOrderViewBuilder {
  def buildOrder(combatState: CombatState): Seq[InitiativeOrderID] = {
    combatState.getInitiativeOrder
  }
}

/**
 * Build an order with the robin head always on the top.
 */
object RobinHeadFirstInitiativeOrderViewBuilder extends InitiativeOrderViewBuilder {
  def buildOrder(combatState: CombatState): Seq[InitiativeOrderID] = {
    val order = combatState.getInitiativeOrder
    val idx: Int = if (combatState.nextUp.isDefined && order.contains(combatState.nextUp.get)) order.indexOf(combatState.nextUp.get) else 0
    order.drop(idx) ++ order.take(idx)
  }
}