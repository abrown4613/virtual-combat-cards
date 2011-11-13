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
package vcc.dnd4e.tracker.transition

import vcc.controller.IllegalActionException
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.event._
import vcc.tracker.Event

abstract class InitiativeCommand extends CombatStateCommand {
  val ioi: InitiativeOrderID

  protected def getTransitions(combatState: CombatState): List[Event[CombatState]]

  def generateEvents(iState: CombatState): List[Event[CombatState]] = {
    if (!iState.order.tracker.isDefinedAt(ioi))
      throw new IllegalActionException(ioi + " is not in sequence")
    getTransitions(iState)
  }
}

case class StartRoundCommand(ioi: InitiativeOrderID) extends InitiativeCommand {
  def getTransitions(iState: CombatState): List[CombatStateEvent] = List(
    InitiativeTrackerUpdateEvent(ioi, InitiativeAction.StartRound),
    EffectListTransformEvent(EffectTransformation.startRound(ioi)))
}

case class EndRoundCommand(ioi: InitiativeOrderID) extends InitiativeCommand {
  protected def getTransitions(combatState: CombatState): List[CombatStateEvent] = {
    val iat = InitiativeTrackerUpdateEvent(ioi, InitiativeAction.EndRound)
    val et = EffectListTransformEvent(EffectTransformation.endRound(ioi))
    if (combatState.lensFactory.initiativeTrackerLens(ioi).get(combatState).state == InitiativeState.Delaying)
      List(iat, et)
    else
      List(iat, RotateRobinEvent, et)
  }
}

case class DelayCommand(ioi: InitiativeOrderID) extends InitiativeCommand {
  protected def getTransitions(combatState: CombatState): List[CombatStateEvent] = {
    val iat = InitiativeTrackerUpdateEvent(ioi, InitiativeAction.DelayAction)
    val et = DelayEffectListTransformEvent(ioi)
    List(iat, RotateRobinEvent, et)
  }
}

case class ReadyActionCommand(ioi: InitiativeOrderID) extends InitiativeCommand {
  protected def getTransitions(combatState: CombatState): List[CombatStateEvent] = {
    List(InitiativeTrackerUpdateEvent(ioi, InitiativeAction.ReadyAction))
  }
}

case class ExecuteReadyCommand(ioi: InitiativeOrderID) extends InitiativeCommand {
  protected def getTransitions(combatState: CombatState): List[CombatStateEvent] = {
    List(InitiativeTrackerUpdateEvent(ioi, InitiativeAction.ExecuteReady), MoveBeforeFirstEvent(ioi))
  }
}

case class MoveUpCommand(ioi: InitiativeOrderID) extends InitiativeCommand {
  protected def getTransitions(combatState: CombatState): List[CombatStateEvent] = {
    List(
      InitiativeTrackerUpdateEvent(ioi, InitiativeAction.MoveUp),
      MoveBeforeFirstEvent(ioi),
      SetRobinEvent(ioi))
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
}