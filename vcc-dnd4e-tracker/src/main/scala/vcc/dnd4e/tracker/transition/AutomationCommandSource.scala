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

import vcc.dnd4e.tracker.common.{InitiativeOrderID, HealthStatus, InitiativeState, CombatState}
import vcc.dnd4e.tracker.common.InitiativeState._
import vcc.dnd4e.tracker.common.HealthStatus._
import vcc.dnd4e.tracker.StateLensFactory
import vcc.tracker.{Command, PartialFunctionCommandStream}

object AutomationCommandSource {

  object HeadStateAndHealth {
    def unapply(state: CombatState): Option[(InitiativeOrderID, InitiativeState.Value, HealthStatus.Value)] = {
      if (state.isCombatStarted)
        get_HeadIOI_InitiativeState_HeathStatus(state)
      else
        None
    }

    private def get_HeadIOI_InitiativeState_HeathStatus(state: CombatState): Some[(InitiativeOrderID, InitiativeState.Value, HealthStatus.Value)] = {
      val ioi = state.order.nextUp.get
      val initState = StateLensFactory.initiativeTrackerLens(ioi).get(state).state
      val health = StateLensFactory.combatantHealth(ioi.combId).get(state).status
      Some((ioi, initState, health))
    }
  }

  val startNextCommandStream = new PartialFunctionCommandStream[CombatState]({
    case HeadStateAndHealth(ioi, Delaying, Dead) => EndRoundCommand(ioi)
    case HeadStateAndHealth(ioi, Waiting, Dead) => StartRoundCommand(ioi)
    case HeadStateAndHealth(ioi, Ready, Dead) => StartRoundCommand(ioi)
    case HeadStateAndHealth(ioi, Acting, Dead) => EndRoundCommand(ioi)
    case s@HeadStateAndHealth(ioi, Waiting, _) => makeNextUpCommand(s, ioi)
    case s@HeadStateAndHealth(ioi, Ready, _) => makeNextUpCommand(s, ioi)
    case HeadStateAndHealth(ioi, Delaying, _) => EndRoundCommand(ioi)
  })

  private def makeNextUpCommand(state: CombatState, next: InitiativeOrderID): Command[CombatState] = {
    val eligible = state.order.sequence.filter(ioi =>
      state.order.tracker(ioi).state == InitiativeState.Delaying &&
        state.roster.combatant(ioi.combId).health.status != HealthStatus.Dead)
    if (eligible.isEmpty) StartRoundCommand(next)
    else NextUpCommand(next, eligible)
  }
}