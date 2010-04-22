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
package vcc.dnd4e.domain.tracker.common

import vcc.controller.message.TransactionalAction

/**
 * This object contains all TransactionalActions for the transactional tracker.
 */
object Command {

  // Combat MetaData Actions
  abstract class TransactionalActionWithMessage(val description: String) extends TransactionalAction

  case class StartCombat() extends TransactionalActionWithMessage("Start combat")

  case class EndCombat() extends TransactionalActionWithMessage("End combat")

  case class AddCombatants(combatants: List[CombatantRosterDefinition])
          extends TransactionalActionWithMessage("Add combatants: " + combatants)

  /**
   * Set initiative for a list of combatants. Will throw exception if the combatant cannot roll initiative.
   * @param initDefinitions Initiative definition for each combatant
   */
  case class SetInitiative(initDefinitions: List[InitiativeDefinition])
          extends TransactionalActionWithMessage("Set initiative: " + initDefinitions)

  /**
   * Set the combat level comment
   */
  case class SetCombatComment(comment: String) extends TransactionalActionWithMessage("Set Combat comment to: " + comment)


  // Initiative Actions
  case class InitiativeAction(who: InitiativeOrderID, action: InitiativeTracker.action.Value)
          extends TransactionalActionWithMessage(who + "executed " + action)

  case class MoveBefore(who: InitiativeOrderID, before: InitiativeOrderID)
          extends TransactionalActionWithMessage(who + " moving before " + before + " in the sequence")

  case class InternalInitiativeAction(who: InitiativeOrderID, action: InitiativeTracker.action.Value)
          extends TransactionalActionWithMessage(who + " executed initiative action " + action)

  // Effect Actions
  case class AddEffect(target: CombatantID, source: CombatantID, condition: Condition, duration: Effect.Duration)
          extends TransactionalActionWithMessage("Add effect: " + condition + " to " + target)

  case class CancelEffect(effectId: EffectID) extends TransactionalActionWithMessage("Cancel effect " + effectId)

  case class SustainEffect(effectId: EffectID) extends TransactionalActionWithMessage("Sustain effect " + effectId)

  case class UpdateEffectCondition(effectId: EffectID, condition: Condition)
          extends TransactionalActionWithMessage("Update effect " + effectId + " to " + condition)

  // Rest Effect
  case class ApplyRest(extended: Boolean) extends TransactionalActionWithMessage(if (extended) "Extended rest" else "Short rest")
}