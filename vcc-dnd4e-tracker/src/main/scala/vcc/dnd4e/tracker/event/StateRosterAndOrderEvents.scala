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
package vcc.dnd4e.tracker.event

import vcc.dnd4e.tracker.common._

//Place holder
private trait StateRosterAndOrderEvents

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

/**
 * Add a combatant to order (setting initiative)
 */
case class AddCombatantToOrderEvent(initDef: InitiativeDefinition) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = iState.lensFactory.orderLens.mod(iState, _.setInitiative(initDef))
}

/**
 * Remove a combatnat form the Initiative order
 */
case class RemoveCombatantFromOrderEvent(cid: CombatantID) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = iState.lensFactory.orderLens.mod(iState, _.removeCombatant(cid))
}

/**
 * Start combat event
 */
case object StartCombatEvent extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = iState.startCombat()
}

/**
 * End Combat Event
 */
case object EndCombatEvent extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = iState.endCombat()
}

/**
 * Remove a single combatant from the Roster and Order
 */
case class RemoveCombatantFromRosterEvent(cid: CombatantID) extends CombatStateEvent {
  //THINK This is the proper semantic, but should we use two events?
  def transition(iState: CombatState): CombatState = {
    val s = iState.lensFactory.rosterLens.mod(iState, r => r.clear(_.definition.cid == cid))
    s.lensFactory.orderLens.mod(s, o => o.removeCombatant(cid))
  }
}

/**
 * Set combat level comment
 */
case class SetCombatCommentEvent(comment: Option[String]) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = iState.copy(comment = comment)
}

/**
 * Set comment for a combatant
 * @param cid Combatant ID to update
 * @param comment Comment to set
 */
case class SetCombatantCommentEvent(cid: CombatantID, comment: String) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = iState.lensFactory.combatantComment(cid).set(iState, comment)
}

/**
 * Rest a combatant
 */
case class RestCombatantEvent(cid: CombatantID, restDuration: RestDuration.Value) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = {
    iState.lensFactory.combatant(cid).mod(iState, c => c.applyRest(restDuration))
  }
}