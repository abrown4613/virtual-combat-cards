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
package vcc.dnd4e.tracker.event

import vcc.dnd4e.tracker.common._
import vcc.tracker.IllegalActionException

/*
* Place holder, not for real use.
*/
private object InitiativeAndEffectEvents

/**
 * Applies effect transformation to all EffectList in the CombatState.
 */
case class EffectListTransformEvent(elt: EffectTransformation) extends CombatStateEvent {

  def transition(iState: CombatState): CombatState = {
    val combIds = iState.roster.entries.keys
    combIds.foldLeft(iState)((st, cid) => {
      iState.lensFactory.combatantEffectList(cid).modIfChanged(st, _.transformAndFilter(elt))
    })
  }
}

/**
 * Handle Delay action on all effects for all combatants.
 */
case class DelayEffectListTransformEvent(ioi: InitiativeOrderID) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = {
    val lf = iState.lensFactory
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
case object RotateRobinEvent extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = iState.lensFactory.orderLens.mod(iState, order => order.rotate())
}

/**
 * Set the robin to a new head (no checks are done)
 */
case class SetRobinEvent(ioi: InitiativeOrderID) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = iState.lensFactory.orderLens.mod(iState, order => order.setNextUp(ioi))
}

/**
 * Compact notation to move a combatant before the first one on the list. Should be called
 * internally.
 */
case class MoveBeforeFirstEvent(ioi: InitiativeOrderID) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = {
    iState.lensFactory.orderLens.mod(iState, order => order.moveBefore(ioi, order.nextUp.get))
  }
}

/**
 * Move before internal transaction
 */
case class MoveBeforeOtherEvent(who: InitiativeOrderID, whom: InitiativeOrderID) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = {
    iState.lensFactory.orderLens.mod(iState, order => order.moveBefore(who, whom))
  }
}

/**
 * This step will update a InitiativeTracker to it's new state. Notice that it does validate if the action is valid.
 */
case class InitiativeTrackerUpdateEvent(who: InitiativeOrderID, action: InitiativeAction.Value) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = {
    val lf = iState.lensFactory
    val ol = lf.orderLens
    val order = ol.get(iState)
    val firstIT = order.tracker(order.nextUp.get)
    val actingLens = lf.initiativeTrackerLens(who)
    if (!iState.rules.canInitiativeOrderPerform(iState, who, action))
      throw new IllegalActionException(who + " can not perform " + action + " current state: " + order.tracker(who) + " first is: " + order.tracker(order.nextUp.get))

    actingLens.mod(iState, it => it.transform(firstIT, action))
  }
}