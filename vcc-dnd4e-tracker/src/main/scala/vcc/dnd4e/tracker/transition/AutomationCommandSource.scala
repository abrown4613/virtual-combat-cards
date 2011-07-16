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

import vcc.tracker.PartialFunctionCommandStream
import vcc.dnd4e.tracker.common.{InitiativeOrderID, HealthStatus, InitiativeTracker, CombatState}
import vcc.dnd4e.tracker.common.InitiativeTracker.state._
import vcc.dnd4e.tracker.common.HealthStatus._
import vcc.dnd4e.tracker.StateLensFactory

object AutomationCommandSource {

  object HeadStateAndHealth {
    def unapply(state: CombatState): Option[(InitiativeOrderID, InitiativeTracker.state.Value, HealthStatus.Value)] = {
      if (state.isCombatStarted) {
        val ioi = state.order.nextUp.get
        val initState = StateLensFactory.initiativeTrackerLens(ioi).get(state).state
        val health = StateLensFactory.combatantHealth(ioi.combId).get(state).status
        Some((ioi, initState, health))
      } else {
        None
      }
    }
  }

  val autoStartDead = new PartialFunctionCommandStream[CombatState, CombatTransition]({
    case HeadStateAndHealth(ioi, Delaying, Dead) => EndRoundTransition(ioi)
    case HeadStateAndHealth(ioi, Waiting, Dead) => StartRoundTransition(ioi)
    case HeadStateAndHealth(ioi, Ready, Dead) => StartRoundTransition(ioi)
    case HeadStateAndHealth(ioi, Acting, Dead) => EndRoundTransition(ioi)
  })

  val autoStartNext = new PartialFunctionCommandStream[CombatState, CombatTransition]({
    case HeadStateAndHealth(ioi, Waiting, health) if (health != Dead) => StartRoundTransition(ioi)
    case HeadStateAndHealth(ioi, Ready, health) if (health != Dead) => StartRoundTransition(ioi)
    case HeadStateAndHealth(ioi, Delaying, health) if (health != Dead) => EndRoundTransition(ioi)
  })
}