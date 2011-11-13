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
package vcc.dnd4e.tracker.common

import vcc.controller.message.TransactionalAction
import vcc.dnd4e.tracker.command._
import vcc.tracker.{SeqCommandStream, CommandStream, Action}

/**
 * This object contains all TransactionalActions for the transactional tracker.
 */
object Command {

  // Combat MetaData Actions
  abstract class TransactionalActionWithMessage(val description: String) extends TransactionalAction with Action[CombatState]

  case class StartCombat() extends TransactionalActionWithMessage("Start combat") {
    def createCommandStream() = {
      singleCommand(StartCombatCommand) followedBy AutomationCommandSource.startNextCommandStream
    }
  }

  case class EndCombat() extends TransactionalActionWithMessage("End combat") {
    def createCommandStream() = singleCommand(EndCombatCommand)
  }

  case class AddCombatants(combatants: List[CombatantRosterDefinition])
    extends TransactionalActionWithMessage("Add combatants: " + combatants) {
    def createCommandStream() = {
      SeqCommandStream(combatants.map(rd => AddCombatantCommand(Option(rd.cid), rd.alias, rd.entity)))
    }
  }

  /**
   * Clear roster of combatants
   * @param all If true will clear Play Combatant and NPC (non-player combatants), when fall only NPC are cleared.
   */
  case class ClearRoster(all: Boolean) extends TransactionalActionWithMessage("Clear " + (if (all) "all" else "enemies") + "combatants") {
    def createCommandStream() = singleCommand(ClearRosterCommand(!all))
  }

  /**
   * Set initiative for a list of combatants. Will throw exception if the combatant cannot roll initiative.
   * @param initDefinitions Initiative definition for each combatant
   */
  case class SetInitiative(initDefinitions: List[InitiativeDefinition])
    extends TransactionalActionWithMessage("Set initiative: " + initDefinitions) {
    def createCommandStream() = {
      SeqCommandStream(initDefinitions.map(i => SetInitiativeCommand(i)))
    }
  }

  /**
   * Set the combat level comment
   */
  case class SetCombatComment(comment: String) extends TransactionalActionWithMessage("Set Combat comment to: " + comment) {
    def createCommandStream() = singleCommand(SetCombatCommentCommand(comment))
  }

  // Initiative Actions
  case class ExecuteInitiativeAction(who: InitiativeOrderID, action: InitiativeAction.Value)
    extends TransactionalActionWithMessage(who + "executed " + action) {
    def createCommandStream() = {
      action match {
        case InitiativeAction.EndRound =>
          singleCommand(EndRoundCommand(who)) followedBy AutomationCommandSource.startNextCommandStream
        case InitiativeAction.DelayAction =>
          singleCommand(DelayCommand(who)) followedBy AutomationCommandSource.startNextCommandStream
        case InitiativeAction.ReadyAction =>
          CommandStream(ReadyActionCommand(who), EndRoundCommand(who)) followedBy AutomationCommandSource.startNextCommandStream
        case InitiativeAction.MoveUp =>
          singleCommand(MoveUpCommand(who))
        case InitiativeAction.StartRound =>
          singleCommand(StartRoundCommand(who))
        case InitiativeAction.ExecuteReady =>
          singleCommand(ExecuteReadyCommand(who))
      }
    }
  }

  case class MoveBefore(who: InitiativeOrderID, before: InitiativeOrderID)
    extends TransactionalActionWithMessage(who + " moving before " + before + " in the sequence") {
    def createCommandStream() = singleCommand(MoveBeforeCommand(who, before))
  }

  // Effect Actions
  case class AddEffect(target: CombatantID, source: CombatantID, condition: Condition, duration: Duration)
    extends TransactionalActionWithMessage("Add effect: " + condition + " to " + target) {
    def createCommandStream() = singleCommand(AddEffectCommand(target, source, condition, duration))
  }

  case class CancelEffect(effectId: EffectID) extends TransactionalActionWithMessage("Cancel effect " + effectId) {
    def createCommandStream() = singleCommand(CancelEffectCommand(effectId))
  }

  case class SustainEffect(effectId: EffectID) extends TransactionalActionWithMessage("Sustain effect " + effectId) {
    def createCommandStream() = singleCommand(SustainEffectCommand(effectId))
  }

  case class UpdateEffectCondition(effectId: EffectID, condition: Condition)
    extends TransactionalActionWithMessage("Update effect " + effectId + " to " + condition) {
    def createCommandStream() = singleCommand(UpdateEffectConditionCommand(effectId, condition))
  }

  // Rest Effect
  case class ApplyRest(extended: Boolean) extends TransactionalActionWithMessage(if (extended) "Extended rest" else "Short rest") {
    def createCommandStream() = singleCommand(RestCommand(extended))
  }

  /**
   * Change the comment for a combatant
   */
  case class SetComment(who: CombatantID, comment: String) extends TransactionalActionWithMessage("Comment of " + who + " has changed") {
    def createCommandStream() = singleCommand(SetCombatantCommentCommand(who, comment))
  }

  //Health Actions
  case class ApplyDamage(who: CombatantID, damage: Int) extends TransactionalActionWithMessage(who + " takes " + damage + " hitpoints of damage") {
    def createCommandStream() = singleCommand(DamageCommand(who, damage))
  }

  case class SetTemporaryHP(who: CombatantID, temporaryHP: Int) extends TransactionalActionWithMessage(who + " recieves " + temporaryHP + " of temporary hitpoints") {
    def createCommandStream() = singleCommand(SetTemporaryHPCommand(who, temporaryHP))
  }

  case class HealDamage(who: CombatantID, heal: Int) extends TransactionalActionWithMessage(who + " recovers " + heal + " hitpoints") {
    def createCommandStream() = singleCommand(HealCommand(who, heal))
  }

  case class FailDeathSave(who: CombatantID) extends TransactionalActionWithMessage(who + " fails save versus death") {
    def createCommandStream() = singleCommand(FailDeathSaveCommand(who))
  }

  case class RevertDeath(who: CombatantID) extends TransactionalActionWithMessage(who + " is no longer dead") {
    def createCommandStream() = singleCommand(RevertDeathCommand(who))
  }

}