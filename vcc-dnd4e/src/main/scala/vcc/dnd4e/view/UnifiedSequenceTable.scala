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
package vcc.dnd4e.view

import vcc.dnd4e.domain.tracker.common.{CombatantID, InitiativeOrderID}
import vcc.dnd4e.domain.tracker.snapshot.CombatState

class UnifiedSequenceTable(val elements: Array[UnifiedCombatant], val state: CombatState) {
  def apply(idx: Int): UnifiedCombatant = elements(idx)

  def apply(combId: CombatantID, orderID: InitiativeOrderID): UnifiedCombatant = {
    val m = elements.find(e => e.combId == combId && orderID == e.orderId)
    if (m.isDefined) m.get else null
  }

  def combatantOption(uid: Option[UnifiedCombatantID]): Option[UnifiedCombatant] =
    if (uid.isDefined)
      elements.find(e => (e.combId == uid.get.combId) && (e.orderId == uid.get.orderId))
    else
      None

  def orderFirst(): Option[UnifiedCombatant] =
    if (state.nextUp.isDefined)
      Some(this(state.nextUp.get.combId, state.nextUp.get))
    else
      None
}
