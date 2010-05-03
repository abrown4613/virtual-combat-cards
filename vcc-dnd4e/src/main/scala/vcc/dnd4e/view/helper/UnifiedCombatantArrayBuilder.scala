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
import vcc.dnd4e.domain.tracker.common._
import vcc.dnd4e.view.UnifiedCombatant

trait InitiativeOrderViewBuilder {
  def buildOrder(combatState: CombatState): Seq[InitiativeOrderID]
}

trait ReserveViewBuilder {
  def buildReserve(combatState: CombatState): Seq[CombatantID]
}

object DontCareReserveViewBuilder extends ReserveViewBuilder {
  def buildReserve(combatState: CombatState): Seq[CombatantID] = {
    combatState.combatantsNotInOrder().toSeq
  }
}

object DirectInitiativeOrderViewBuilder extends InitiativeOrderViewBuilder {
  def buildOrder(combatState: CombatState): Seq[InitiativeOrderID] = {
    combatState.order
  }
}

/**
 * This service object will used a ReserveViewBuilder and a InitiativeOrderViewBuilder to build a unified array with all
 * the combatant in a single Array.
 */
object UnifiedCombatantArrayBuilder {
  //TODO Maybe fold this into UnifiedSequenceTable
  def buildList(combatState: CombatState, orderBuilder: InitiativeOrderViewBuilder, reserveBuilder: ReserveViewBuilder): Array[UnifiedCombatant] = {
    val order = orderBuilder.buildOrder(combatState).map(e => new UnifiedCombatant(e.combId, combatState.initiativeTrackerFromID(e), combatState.combatantViewFromID(e.combId)))
    val reserve = reserveBuilder.buildReserve(combatState).map(e => new UnifiedCombatant(e, null, combatState.combatantViewFromID(e)))
    (order ++ reserve).toArray
  }
}