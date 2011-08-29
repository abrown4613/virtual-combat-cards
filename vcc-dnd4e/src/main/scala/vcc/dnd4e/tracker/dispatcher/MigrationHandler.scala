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

import vcc.dnd4e.domain.tracker.transactional.AbstractCombatController
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.domain.tracker.common.Command._
import vcc.dnd4e.tracker.common.{InitiativeAction => action}
import org.slf4j.Logger

trait MigrationHandler {
  this: AbstractCombatController =>

  private val migrationLogger = org.slf4j.LoggerFactory.getLogger("infra")

  addRewriteRule {
    case ExecuteInitiativeAction(it, action.DelayAction) =>
      Seq(InternalInitiativeAction(it, action.StartRound), InternalInitiativeAction(it, action.DelayAction))

    case ExecuteInitiativeAction(it, action.ReadyAction) =>
      Seq(InternalInitiativeAction(it, action.ReadyAction), InternalInitiativeAction(it, action.EndRound))

    case ExecuteInitiativeAction(it, action) => Seq(InternalInitiativeAction(it, action))
  }

  def dumpState(state: CombatState, os: Logger) {
    os.debug("\tOrder: " + state.order)
    os.debug("\tCombs: " + state.roster)
  }

  addHandler {
    case action =>
      val ts = ActionTranslator.translate(action)
      migrationLogger.debug("Action: {}", action)
      migrationLogger.debug("Mapped to {}: ", ts.mkString(" + "))
      val oldState = context.iState.value
      context.iState.value = ts.foldLeft(context.iState.value)((s, t) => s.transitionWith(t.generateTransitions(s)))
      migrationLogger.debug("   New State: ")
      dumpState(context.iState.value, migrationLogger)
      // Check if we need to advance dead if we rotated
      if (oldState.order.nextUp != context.iState.value.order.nextUp) {
        //We know we have someone new as the nextUp (first in order)
        //Now check if we just ended some one else and auto start if next is dead
        action match {
          case InternalInitiativeAction(_, initAction) if ((initAction == InitiativeAction.EndRound || initAction == InitiativeAction.DelayAction)) => {
            val state = context.iState.value
            val nextIOI = state.order.nextUp.get
            val nextIT = state.order.tracker(nextIOI)
            val health = context.combatantFromID(nextIOI.combId).health
            if (health.status == HealthStatus.Dead) {
              if (nextIT.state == InitiativeState.Delaying)
                enqueueAction(InternalInitiativeAction(nextIT.orderID, InitiativeAction.EndRound))
              enqueueAction(InternalInitiativeAction(nextIT.orderID, InitiativeAction.StartRound))
              enqueueAction(InternalInitiativeAction(nextIT.orderID, InitiativeAction.EndRound))
            }
          }
          case _ =>
        }
      }
  }
}