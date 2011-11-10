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
package vcc.dnd4e.tracker.dispatcher

import vcc.controller.message.TransactionalAction
import vcc.dnd4e.tracker.common.Command._
import vcc.dnd4e.tracker.transition._
import vcc.dnd4e.tracker.common.{CombatState, InitiativeAction}
import vcc.tracker.{StateCommand, CommandStream, ActionStreamTranslator}
import vcc.tracker.SeqCommandStream
import vcc.dnd4e.tracker.transition.AutomationCommandSource._

object ActionTranslator extends ActionStreamTranslator[CombatState, TransactionalAction] {
  implicit def transition2TransitionList(t: CombatStateCommand) = List(t)

  def translate(action: TransactionalAction): List[CombatStateCommand] = {
    action match {

      //Damage action
      case ApplyDamage(who, amount) => DamageCommand(who, amount)
      case HealDamage(who, amount) => HealCommand(who, amount)
      case SetTemporaryHP(who, amount) => SetTemporaryHPCommand(who, amount)
      case FailDeathSave(who) => FailDeathSaveCommand(who)
      case RevertDeath(who) => RevertDeathCommand(who)
      case SetComment(who, comment) => SetCombatantCommentCommand(who, comment)

      //Order actions
      case ExecuteInitiativeAction(who, initAction) =>
        initAction match {
          case InitiativeAction.StartRound => StartRoundCommand(who)
          case InitiativeAction.EndRound => EndRoundCommand(who)
          case InitiativeAction.DelayAction => DelayCommand(who)
          case InitiativeAction.MoveUp => MoveUpCommand(who)
          case InitiativeAction.ExecuteReady => ExecuteReadyCommand(who)
          case InitiativeAction.ReadyAction => ReadyActionCommand(who)
        }

      case MoveBefore(who, whom) => MoveBeforeCommand(who, whom);

      // Effect messages
      case AddEffect(target, source, condition, duration) => AddEffectCommand(target, source, condition, duration)
      case UpdateEffectCondition(effectId, condition) => UpdateEffectConditionCommand(effectId, condition)
      case CancelEffect(eid) => CancelEffectCommand(eid)
      case SustainEffect(eid) => SustainEffectCommand(eid)

      //Combat Meta
      case AddCombatants(combs) => combs.map(rd => AddCombatantCommand(Option(rd.cid), rd.alias, rd.entity))
      case SetInitiative(iDefs) => iDefs.map(i => SetInitiativeCommand(i))
      case SetCombatComment(comment) => SetCombatCommentCommand(comment)
      case ApplyRest(extended) => RestCommand(extended)
      case StartCombat() => StartCombatCommand
      case EndCombat() => EndCombatCommand
      case ClearRoster(all) => ClearRosterCommand(!all)
      case _ => throw new Exception("Cant handle " + action)
    }
  }

  private def seqStream(s: StateCommand[CombatState]*): CommandStream[CombatState, StateCommand[CombatState]] = {
    SeqCommandStream(s)
  }

  def translateToCommandStream(action: TransactionalAction): CommandStream[CombatState, StateCommand[CombatState]] = {
    action match {
      case StartCombat() =>
        seqStream(StartCombatCommand) followedBy  autoStartDead followedBy autoStartNext
      case ExecuteInitiativeAction(who, InitiativeAction.EndRound) =>
        seqStream(EndRoundCommand(who)) followedBy autoStartDead followedBy autoStartNext
      case ExecuteInitiativeAction(who, InitiativeAction.ReadyAction) =>
        seqStream(ReadyActionCommand(who), EndRoundCommand(who)) followedBy autoStartDead followedBy autoStartNext
      case ExecuteInitiativeAction(who, InitiativeAction.DelayAction) =>
        seqStream(DelayCommand(who)) followedBy autoStartDead followedBy autoStartNext
      case s => SeqCommandStream(ActionTranslator.translate(action))
    }
  }
}