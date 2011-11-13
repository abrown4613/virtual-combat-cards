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
package vcc.dnd4e.domain.tracker.snapshot

import vcc.dnd4e.domain.tracker.common._
import vcc.dnd4e.tracker.common.{InitiativeTracker, InitiativeOrderID, CombatantID}

/**
 * A snapshot of a CombatState, this is limit version of the transactional state.
 */
case class SnapshotCombatState(
                        isCombatStarted: Boolean,
                        combatComment: String,
                        private val order: List[InitiativeOrderID],
                        initiatives: Map[InitiativeOrderID, InitiativeTracker],
                        nextUp: Option[InitiativeOrderID],
                        roster: Map[CombatantID, CombatantStateView]) extends CombatStateView {
  def initiativeTrackerFromID(orderId: InitiativeOrderID): InitiativeTracker = initiatives(orderId)

  def getInitiativeOrder: List[InitiativeOrderID] = order

  def combatantViewFromID(id: CombatantID): CombatantStateView = roster(id)

  def allCombatantIDs: List[CombatantID] = roster.keys.toList

  def combatantsNotInOrder(): Set[CombatantID] = Set((roster.keys.toList filterNot (order.map(_.combId) contains)): _*)

}