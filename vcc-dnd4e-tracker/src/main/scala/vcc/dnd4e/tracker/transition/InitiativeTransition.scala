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
package vcc.dnd4e.tracker.transition

import vcc.dnd4e.tracker.StateLensFactory
import vcc.controller.IllegalActionException
import vcc.dnd4e.tracker.common.{EffectTransformation, CombatState, InitiativeTracker, InitiativeOrderID}

/*
 * Place holder, not for real use.
 */
private object InitiativeTransition

/**
 * Applies effect transformation to all EffectList in the CombatState.
 */
private[transition] case class EffectListTransformStep(elt: EffectTransformation) extends CombatTransition {

  def transition(lf: StateLensFactory, iState: CombatState): CombatState = {
    val combIds = iState.roster.entries.keys
    combIds.foldLeft(iState)((st, cid) => {
      lf.combatantEffectList(cid).modIfChanged(st, _.transformAndFilter(elt))
    })
  }
}

/**
 * Handle Delay action on all effects for all combatants.
 */
private[transition] case class DelayEffectListTransformStep(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState = {
    val combIds = iState.roster.entries.keys
    combIds.foldLeft(iState)((st, cid) => {
      val ally = iState.rules.areAllied(iState, ioi.combId, cid)
      lf.combatantEffectList(cid).modIfChanged(st, _.transformAndFilter(EffectTransformation.processDelay(ally, ioi)))
    })
  }
}

/**
 * Simple Robin Rotation
 */
private[transition] case object RotateRobinStep extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState = lf.orderLens.mod(iState, order => order.rotate())
}

/**
 * Set the robin to a new head (no checks are done)
 */
private[transition] case class SetRobinStep(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState = lf.orderLens.mod(iState, order => order.setNextUp(ioi))
}

/**
 * Compact notation to move a combatant before the first one on the list. Should be called
 * internally.
 */
private[transition] case class MoveBeforeFirstStep(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState = {
    lf.orderLens.mod(iState, order => order.moveBefore(ioi, order.nextUp.get))
  }
}

/**
 * Move before internal transaction
 */
private[transition] case class MoveBeforeOtherStep(who: InitiativeOrderID, whom: InitiativeOrderID) extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState = {
    lf.orderLens.mod(iState, order => order.moveBefore(who, whom))
  }
}

/**
 * This step will update a InitiativeTracker to it's new state. Notice that it does validate if the action is valid.
 */
private[transition] case class InitiativeTrackerUpdateStep(who: InitiativeOrderID, action: InitiativeTracker.action.Value) extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState = {
    val ol = lf.orderLens
    val order = ol.get(iState)
    val firstIT = order.tracker(order.nextUp.get)
    val actingLens = lf.initiativeTrackerLens(who)
    if (!iState.rules.canInitiativeOrderPerform(iState, who, action))
      throw new IllegalActionException(who + " can not perform " + action + " current state: " + order.tracker(who) + " first is: " + order.tracker(order.nextUp.get))

    actingLens.mod(iState, it => it.transform(firstIT, action))
  }
}


case class StartRoundTransition(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState = {
    val iat = InitiativeTrackerUpdateStep(ioi, InitiativeTracker.action.StartRound)
    val et = EffectListTransformStep(EffectTransformation.startRound(ioi))
    iState.transitionWith(List(iat, et))
  }
}

case class EndRoundTransition(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState = {
    val iat = InitiativeTrackerUpdateStep(ioi, InitiativeTracker.action.EndRound)
    val et = EffectListTransformStep(EffectTransformation.endRound(ioi))
    iState.transitionWith(
      if (lf.initiativeTrackerLens(ioi).get(iState).state == InitiativeTracker.state.Delaying)
        List(iat, et)
      else
        List(iat, RotateRobinStep, et)
    )
  }
}

case class DelayTransition(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState = {
    val iat = InitiativeTrackerUpdateStep(ioi, InitiativeTracker.action.Delay)
    val et = DelayEffectListTransformStep(ioi)
    iState.transitionWith(List(iat, RotateRobinStep, et))
  }
}

case class ReadyActionTransition(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState = {
    iState.transitionWith(List(InitiativeTrackerUpdateStep(ioi, InitiativeTracker.action.Ready)))
  }
}

case class ExecuteReadyTransition(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState = {
    val iat = InitiativeTrackerUpdateStep(ioi, InitiativeTracker.action.ExecuteReady)
    val mb = MoveBeforeFirstStep(ioi)
    iState.transitionWith(List(iat, mb))
  }
}

case class MoveUpTransition(ioi: InitiativeOrderID) extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState = {
    iState.transitionWith(List(
      InitiativeTrackerUpdateStep(ioi, InitiativeTracker.action.MoveUp),
      MoveBeforeFirstStep(ioi),
      SetRobinStep(ioi)))
  }
}

case class MoveBeforeTransition(who: InitiativeOrderID, whom: InitiativeOrderID) extends CombatTransition {
  def transition(lf: StateLensFactory, state: CombatState): CombatState = {
    if (!state.rules.canMoveBefore(state, who, whom))
      throw new IllegalActionException("Cant move " + who + " before " + whom)
    // Advance to second if first moved out (you must have 2 elements because of the rules.canMoveBefore
    val ol = lf.orderLens
    if (who == ol.get(state).nextUp.get)
      state.transitionWith(List(RotateRobinStep, MoveBeforeOtherStep(who, whom)))
    else
      state.transitionWith(List(MoveBeforeOtherStep(who, whom)))
  }
}

