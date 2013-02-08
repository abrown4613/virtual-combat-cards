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


/**
 * This identifier is used to search for UnifiedCombatant.
 * @param combId A valid CombatantID, this is always present
 * @param orderId An optional InitiativeOrderID that identifies the UnifiedCombatant as in initiative order, null if the
 * combatant is not in the initiative order.
 */
case class UnifiedCombatantID(combId: CombatantID, orderId: InitiativeOrderID)

/**
 * This is a wrapper for the CombatantStateView with some simplifications to allow uniform access in the GUI components.
 * All will have a combId, and if they belong to the InitiativeOrder they also have an Initiative.
 * Several getter methods are provided to access the underlying CombatantStateView.
 */
class UnifiedCombatant(val combId: CombatantID,
                       val initiative: InitiativeTracker,
                       combatant: Combatant) {
  def isCharacter = combatant.combatantType == CombatantType.Character

  def health = combatant.health

  def effects = combatant.effects

  def comment = combatant.comment

  def definition = combatant.definition

  def name = combatant.definition.entity.name

  def alias = combatant.definition.alias

  def matches(ucid: UnifiedCombatantID) = ((ucid.combId == combId) && (ucid.orderId == orderId))

  def matches(opt: Option[UnifiedCombatantID]): Boolean = if (opt.isDefined) this.matches(opt.get) else false

  def matches(uc: UnifiedCombatant) = (uc.combId == combId) && (uc.orderId == orderId)

  def matches(oid: InitiativeOrderID): Boolean = (oid != null) && (orderId == oid)

  def unifiedId = UnifiedCombatantID(combId, this.orderId)

  def orderId: InitiativeOrderID = if (initiative != null) initiative.orderID else null

  def isInOrder = (initiative != null)

  override def toString: String = "UnifiedCombatant(" + combId.id + "/" + (if (orderId != null) orderId.toLabelString else "nio") + ")"
}