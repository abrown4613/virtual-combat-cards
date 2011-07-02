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
//$Id$
package vcc.dnd4e.tracker.dispatcher

import vcc.controller.message.TransactionalAction
import vcc.dnd4e.domain.tracker.common.Command._
import vcc.dnd4e.tracker.transition._
import vcc.dnd4e.tracker.common.InitiativeTracker

object ActionTranslator {
  implicit def transition2TransitionList(t: CombatTransition) = List(t)

  def translate(action: TransactionalAction): List[CombatTransition] = {
    action match {

      //Damage action
      case ApplyDamage(who, amount) => DamageTransition(who, amount)
      case HealDamage(who, amount) => HealTransition(who, amount)
      case SetTemporaryHP(who, amount) => SetTemporaryHPTransition(who, amount)
      case FailDeathSave(who) => FailDeathSaveTransition(who)
      case RevertDeath(who) => RevertDeathTransition(who)
      case SetComment(who, comment) => SetCombatantCommentTransition(who, comment)

      //Order actions
      case InternalInitiativeAction(who, initAction) =>
        initAction match {
          case InitiativeTracker.action.StartRound => StartRoundTransition(who)
          case InitiativeTracker.action.EndRound => EndRoundTransition(who)
          case InitiativeTracker.action.Delay => DelayTransition(who)
          case InitiativeTracker.action.MoveUp => MoveUpTransition(who)
          case InitiativeTracker.action.ExecuteReady => ExecuteReadyTransition(who)
          case InitiativeTracker.action.Ready => ReadyActionTransition(who)
        }

      case MoveBefore(who, whom) => MoveBeforeTransition(who, whom);

      // Effect messages
      case AddEffect(target, source, condition, duration) => AddEffectTransition(target, source, condition, duration)
      case UpdateEffectCondition(effectId, condition) => UpdateEffectConditionTransition(effectId, condition)
      case CancelEffect(eid) => CancelEffectTransition(eid)
      case SustainEffect(eid) => SustainEffectTransition(eid)

      //Combat Meta
      case AddCombatants(combs) => combs.map(rd => AddCombatantTransition(Option(rd.cid), rd.alias, rd.entity))
      case SetInitiative(iDefs) => iDefs.map(i => SetInitiativeTransition(i))
      case SetCombatComment(comment) => SetCombatCommentTransition(comment)
      case ApplyRest(extended) => RestTransition(extended)
      case StartCombat() => StartCombatTransition
      case EndCombat() => EndCombatTransition
      case ClearRoster(all) => ClearRosterTransition(!all)
      case _ => throw new Exception("Cant handle " + action)
    }
  }
}