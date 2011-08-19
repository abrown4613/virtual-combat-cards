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

/*
* Place holder, not for real use.
*/
private object InitiativeTransition

case class StartRoundTransition(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(iState: CombatState): CombatState = {
    val iat = InitiativeTrackerUpdateEvent(ioi, InitiativeAction.StartRound)
    val et = EffectListTransformEvent(EffectTransformation.startRound(ioi))
    iState.transitionWith(List(iat, et))
  }
}

case class EndRoundTransition(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(iState: CombatState): CombatState = {
    val iat = InitiativeTrackerUpdateEvent(ioi, InitiativeAction.EndRound)
    val et = EffectListTransformEvent(EffectTransformation.endRound(ioi))
    iState.transitionWith(
      if (iState.lensFactory.initiativeTrackerLens(ioi).get(iState).state == InitiativeState.Delaying)
        List(iat, et)
      else
        List(iat, RotateRobinEvent, et)
    )
  }
}

case class DelayTransition(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(iState: CombatState): CombatState = {
    val iat = InitiativeTrackerUpdateEvent(ioi, InitiativeAction.DelayAction)
    val et = DelayEffectListTransformEvent(ioi)
    iState.transitionWith(List(iat, RotateRobinEvent, et))
  }
}

case class ReadyActionTransition(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(iState: CombatState): CombatState = {
    iState.transitionWith(List(InitiativeTrackerUpdateEvent(ioi, InitiativeAction.ReadyAction)))
  }
}

case class ExecuteReadyTransition(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(iState: CombatState): CombatState = {
    val iat = InitiativeTrackerUpdateEvent(ioi, InitiativeAction.ExecuteReady)
    val mb = MoveBeforeFirstEvent(ioi)
    iState.transitionWith(List(iat, mb))
  }
}

case class MoveUpTransition(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(iState: CombatState): CombatState = {
    iState.transitionWith(List(
      InitiativeTrackerUpdateEvent(ioi, InitiativeAction.MoveUp),
      MoveBeforeFirstEvent(ioi),
      SetRobinEvent(ioi)))
  }
}

case class MoveBeforeTransition(who: InitiativeOrderID, whom: InitiativeOrderID) extends CombatTransition {
  def transition(state: CombatState): CombatState = {
    if (!state.rules.canMoveBefore(state, who, whom))
      throw new IllegalActionException("Cant move " + who + " before " + whom)
    // Advance to second if first moved out (you must have 2 elements because of the rules.canMoveBefore
    val ol = state.lensFactory.orderLens
    if (who == ol.get(state).nextUp.get)
      state.transitionWith(List(RotateRobinEvent, MoveBeforeOtherEvent(who, whom)))
    else
      state.transitionWith(List(MoveBeforeOtherEvent(who, whom)))
  }
}

