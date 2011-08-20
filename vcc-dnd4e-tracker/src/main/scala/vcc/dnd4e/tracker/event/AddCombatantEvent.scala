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
package vcc.dnd4e.tracker.event

import vcc.dnd4e.tracker.common._

/**
 * Add combatant to the combat.
 * @param cid Optional ID for the combatant, None is not relevant
 * @param alias Option alias for combatant (null if not present)
 * @param entity CombatantEntity definition
 */
case class AddCombatantEvent(cid: Option[CombatantID], alias: String, entity: CombatantEntity) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = {
    val rl = iState.lensFactory.rosterLens
    rl.set(iState, rl.get(iState).addCombatant(cid, alias, entity))
  }
}

case class AddCombatantToOrderEvent(initDef: InitiativeDefinition) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = iState.lensFactory.orderLens.mod(iState, _.setInitiative(initDef))
}

case class RemoveCombatantFromOrderEvent(cid: CombatantID) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = iState.lensFactory.orderLens.mod(iState, _.removeCombatant(cid))
}

case object StartCombatEvent extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = iState.startCombat()
}

case object EndCombatEvent extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = iState.endCombat()
}