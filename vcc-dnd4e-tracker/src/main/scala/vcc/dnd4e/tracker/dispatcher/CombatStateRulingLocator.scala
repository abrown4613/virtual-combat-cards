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

import vcc.dnd4e.tracker.common._
import vcc.tracker.{StateCommand, Ruling, RulingLocationService}
import vcc.dnd4e.tracker.transition.{EndRoundCommand, StartRoundCommand, NextUpCommand}
import vcc.dnd4e.tracker.ruling._
import vcc.dnd4e.tracker.common.ConditionMatcher

object CombatStateRulingLocator extends RulingLocationService[CombatState] {

  type R = Ruling[CombatState, _, _, _]

  def rulingsFromStateWithCommand(state: CombatState, command: StateCommand[CombatState]): List[Ruling[CombatState, _, _, _]] = {
    command match {
      case NextUpCommand(first, eligible) => List(NextUpRuling(EligibleNext(first, eligible), None))
      case EndRoundCommand(who) => searchEndRound(state, who.combId)
      case StartRoundCommand(who) => searchStartRound(state, who.combId)
      case _ => Nil
    }
  }

  private def endRoundMatcher(who: CombatantID): PartialFunction[Effect, R] = {
    case effect@Effect(EffectID(`who`, n), _, _, Duration.SaveEnd) =>
      (SaveRuling(Save.Against(effect.effectId, effect.condition.description), None))
    case effect@Effect(EffectID(`who`, n), _, _, Duration.SaveEndSpecial) =>
      SaveSpecial.rulingFromEffect(effect)
    case effect@Effect(eid, _, condition, Duration.RoundBound(InitiativeOrderID(`who`, _), Duration.Limit.EndOfTurnSustain)) =>
      SustainEffect.fromEffect(effect)
  }

  private def searchEndRound(state: CombatState, who: CombatantID): List[R] = {
    val whoMatcher = endRoundMatcher(who)
    state.getAllEffects.flatMap(whoMatcher.lift(_)) ::: dyingRuling(state, who)
  }


  private def startRoundMatcher(who: CombatantID): PartialFunction[Effect, R] = {
    case Effect(eid@EffectID(`who`, n), _, Effect.Condition.Generic(ConditionMatcher.FirstOngoing(full, hint), _), _) =>
      (OngoingDamageRuling(OngoingDamage.CausedBy(eid), None))
    case Effect(eid@EffectID(`who`, n), _, Effect.Condition.Generic(ConditionMatcher.FirstRegenerate(full, hint), _), _) =>
      (RegenerationRuling(CausedBy(eid), None))
  }


  private def searchStartRound(state: CombatState, who: CombatantID): List[Ruling[CombatState, _, _, _]] = {
    val whoMatcher = startRoundMatcher(who)
    val (regen, rest) = state.getAllEffects.flatMap(whoMatcher.lift(_)).toList.partition(x => x.isInstanceOf[RegenerationRuling])
    (regen ::: rest)
  }

  private def dyingRuling(state: CombatState, who: CombatantID): List[CombatStateRulingLocator.R] = {
    if (state.combatant(who).health.status == HealthStatus.Dying)
      List(SaveVersusDeathRuling(SaveVersusDeath.Dying(who), None))
    else Nil
  }
}
