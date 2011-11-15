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
package vcc.dnd4e.tracker.command

import vcc.controller.IllegalActionException
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.event._
import vcc.tracker.{Ruling, Event}
import vcc.dnd4e.tracker.ruling._

abstract class InitiativeCommand extends CombatStateCommand {
  val who: InitiativeOrderID

  protected def getTransitions(combatState: CombatState): List[Event[CombatState]]

  def generateEvents(iState: CombatState): List[Event[CombatState]] = {
    if (!iState.order.tracker.isDefinedAt(who))
      throw new IllegalActionException(who + " is not in sequence")
    getTransitions(iState)
  }
}

case class StartRoundCommand(who: InitiativeOrderID) extends InitiativeCommand {
  def getTransitions(iState: CombatState): List[CombatStateEvent] = List(
    InitiativeTrackerUpdateEvent(who, InitiativeAction.StartRound),
    EffectListTransformEvent(EffectTransformation.startRound(who)))

  override def requiredRulings(state: CombatState) = searchStartRound(state, who.combId)

  private def startRoundMatcher(who: CombatantID): PartialFunction[Effect, Ruling[CombatState, _, _]] = {
    case Effect(eid@EffectID(`who`, n), _, Effect.Condition.Generic(ConditionMatcher.FirstOngoing(full, hint), _), _) =>
      (OngoingDamageRuling(eid, None))
    case Effect(eid@EffectID(`who`, n), _, Effect.Condition.Generic(ConditionMatcher.FirstRegenerate(full, hint), _), _) =>
      (RegenerationRuling(eid, None))
  }

  private def searchStartRound(state: CombatState, who: CombatantID): List[Ruling[CombatState, _, _]] = {
    val whoMatcher = startRoundMatcher(who)
    val (regen, rest) = state.getAllEffects.flatMap(whoMatcher.lift(_)).toList.partition(x => x.isInstanceOf[RegenerationRuling])
    (regen ::: rest)
  }
}

case class EndRoundCommand(who: InitiativeOrderID) extends InitiativeCommand {
  protected def getTransitions(combatState: CombatState): List[CombatStateEvent] = {
    val iat = InitiativeTrackerUpdateEvent(who, InitiativeAction.EndRound)
    val et = EffectListTransformEvent(EffectTransformation.endRound(who))
    if (combatState.lensFactory.initiativeTrackerLens(who).get(combatState).state == InitiativeState.Delaying)
      List(iat, et)
    else
      List(iat, RotateRobinEvent, et)
  }

  override def requiredRulings(state: CombatState): List[Ruling[CombatState, _, _]] = searchEndRound(state, who.combId)

  private def endRoundMatcher(who:  CombatantID): PartialFunction[Effect, Ruling[CombatState, _, _]] = {
    case effect@Effect(EffectID(`who`, n), _, _, Duration.SaveEnd) =>
      (SaveRuling(effect.effectId, None))
    case effect@Effect(EffectID(`who`, n), _, _, Duration.SaveEndSpecial) =>
      SaveSpecialRuling.rulingFromEffect(effect)
    case effect@Effect(eid, _, condition, Duration.RoundBound(InitiativeOrderID(`who`, _), Duration.Limit.EndOfTurnSustain)) =>
      SustainEffectRuling(effect.effectId, None)
  }

  private def searchEndRound(state: CombatState, who: CombatantID): List[Ruling[CombatState, _, _]] = {
    val whoMatcher = endRoundMatcher(who)
    state.getAllEffects.flatMap(whoMatcher.lift(_)) ::: dyingRuling(state, who)
  }


  private def dyingRuling(state: CombatState, who: CombatantID): List[Ruling[CombatState, _, _]] = {
    if (state.combatant(who).health.status == HealthStatus.Dying)
      List(SaveVersusDeathRuling(who, None))
    else Nil
  }
}

case class DelayCommand(who: InitiativeOrderID) extends InitiativeCommand {
  protected def getTransitions(combatState: CombatState): List[CombatStateEvent] = {
    val iat = InitiativeTrackerUpdateEvent(who, InitiativeAction.DelayAction)
    val et = DelayEffectListTransformEvent(who)
    List(iat, RotateRobinEvent, et)
  }
}

case class ReadyActionCommand(who: InitiativeOrderID) extends InitiativeCommand {
  protected def getTransitions(combatState: CombatState): List[CombatStateEvent] = {
    List(InitiativeTrackerUpdateEvent(who, InitiativeAction.ReadyAction))
  }
}

case class ExecuteReadyCommand(who: InitiativeOrderID) extends InitiativeCommand {
  protected def getTransitions(combatState: CombatState): List[CombatStateEvent] = {
    List(InitiativeTrackerUpdateEvent(who, InitiativeAction.ExecuteReady), MoveBeforeFirstEvent(who))
  }
}

case class MoveUpCommand(who: InitiativeOrderID) extends InitiativeCommand {
  protected def getTransitions(combatState: CombatState): List[CombatStateEvent] = {
    List(
      InitiativeTrackerUpdateEvent(who, InitiativeAction.MoveUp),
      MoveBeforeFirstEvent(who),
      SetRobinEvent(who))
  }
}

case class MoveBeforeCommand(who: InitiativeOrderID, whom: InitiativeOrderID) extends CombatStateCommand {
  def generateEvents(iState: CombatState): List[Event[CombatState]] = {
    def moveOutFirst: List[CombatStateEvent] = List(RotateRobinEvent, MoveBeforeOtherEvent(who, whom))
    def moveOtherOut: List[CombatStateEvent] = List(MoveBeforeOtherEvent(who, whom))

    if (!iState.order.tracker.isDefinedAt(who))
      throw new IllegalActionException(who + " is not in sequence")

    if (!iState.order.tracker.isDefinedAt(whom))
      throw new IllegalActionException(whom + " is not in sequence")

    if (!iState.rules.canMoveBefore(iState, who, whom))
      throw new IllegalActionException("Cant move " + who + " before " + whom)

    val ol = iState.lensFactory.orderLens

    if (who == ol.get(iState).nextUp.get) moveOutFirst
    else moveOtherOut
  }
}

case class NextUpCommand(next: InitiativeOrderID, delaying: List[InitiativeOrderID]) extends CombatStateCommand {
  def generateEvents(state: CombatState): List[Event[CombatState]] = Nil

  override def requiredRulings(state: CombatState): List[Ruling[CombatState, _, _]] = {
    List(NextUpRuling(this, None))
  }
}