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
package vcc.dnd4e.domain.tracker.transactional

import vcc.dnd4e.domain.tracker.common._
import vcc.dnd4e.domain.tracker.common.Command._
import InitiativeTracker.{action}
import vcc.controller.IllegalActionException

trait InitiativeActionHandler {
  this: AbstractCombatController =>

  addRewriteRule {
    case InitiativeAction(context.initTrackerFromID(it), action.Delay) =>
      Seq(InternalInitiativeAction(it.orderID, action.StartRound), InternalInitiativeAction(it.orderID, action.Delay))

    case InitiativeAction(context.initTrackerFromID(it), action.Ready) =>
      Seq(InternalInitiativeAction(it.orderID, action.Ready), InternalInitiativeAction(it.orderID, action.EndRound))

    case InitiativeAction(context.initTrackerFromID(it), action) => Seq(InternalInitiativeAction(it.orderID, action))
  }

  addHandler {

    case InternalInitiativeAction(who, action) =>
      // First thing check if we can perform this
      println("Execute " + who + " action:" + action)
      if (!rules.canInitiativeOrderPerform(context, who, action))
        throw new IllegalActionException(who + " can not perform " + action + " current state: " + context.order.initiativeTrackerFor(who) + " first is: " + context.order.robinHeadInitiativeTracker())
      val firstIT = context.order.robinHeadInitiativeTracker()
      val actingIT = context.order.initiativeTrackerFor(who)
      val actingNewIT = actingIT.transform(firstIT, action)
      context.order.updateInitiativeTrackerFor(who, actingNewIT)

      action match {
        case InitiativeTracker.action.EndRound =>
          if (actingIT.state != InitiativeTracker.state.Delaying)
            context.order.rotate()
        case InitiativeTracker.action.Delay => context.order.rotate()
        case InitiativeTracker.action.MoveUp =>
          context.order.moveBefore(who, firstIT.orderID)
          context.order.setRobinHead(who)
        case InitiativeTracker.action.ExecuteReady =>
          context.order.moveBefore(who, firstIT.orderID)
        case _ => //Nothing to do
      }

      // Check if we need to advance dead if we rotated
      if ((action == InitiativeTracker.action.EndRound || action == InitiativeTracker.action.Delay)
              && (firstIT.orderID != context.order.robinHeadInitiativeTracker.orderID)) {
        val nextIT = context.order.robinHeadInitiativeTracker()
        val health = context.roster.combatant(nextIT.orderID.combId).health
        if (health.status == HealthTracker.Status.Dead) {
          if (nextIT.state == InitiativeTracker.state.Delaying)
            enqueueAction(InternalInitiativeAction(nextIT.orderID, InitiativeTracker.action.EndRound))
          enqueueAction(InternalInitiativeAction(nextIT.orderID, InitiativeTracker.action.StartRound))
          enqueueAction(InternalInitiativeAction(nextIT.orderID, InitiativeTracker.action.EndRound))
        }
      }

    case a@MoveBefore(context.initTrackerFromID(who), context.initTrackerFromID(whom)) =>
      if (!rules.canMoveBefore(context, who.orderID, whom.orderID))
        throw new IllegalActionException("Cant move " + who + " before " + whom)
      // Advance to second if first moved out (you must have 2 elements because of the rules.canMoveBefore
      if (who.orderID == context.order.robinHeadInitiativeTracker.orderID) context.order.rotate()
      context.order.moveBefore(who.orderID, whom.orderID)

  }
}