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

import vcc.dnd4e.tracker.common.{CombatantID, CombatState, SampleStateData, InitiativeDefinition}

/**
 * Library of Event-Sourcing events to help build behaviour test
 */
trait EventSourceSampleEvents extends SampleStateData {
  protected val evtAddCombA = AddCombatantEvent(Some(combA), null, entityPc1)
  protected val evtAddCombB = AddCombatantEvent(Some(combB), null, entityPc2)
  protected val evtAddCombNoId = AddCombatantEvent(None, null, entityMinion)
  protected val evtAddMonsterNoId = AddCombatantEvent(None, null, entityMonster)
  protected val evtAddComb2 = AddCombatantEvent(Some(comb2), null, entityMonster)
  protected val emptyState = CombatState.empty
  protected val evtInitA = AddCombatantToOrderEvent(InitiativeDefinition(combA, 5, List(10)))
  protected val evtStart = StartCombatEvent
  protected val evtEnd = EndCombatEvent

  protected def meAddToOrder(cid: CombatantID, bonus: Int, init: Int*) = AddCombatantToOrderEvent(InitiativeDefinition(cid, bonus, init.toList))

  protected def killEvent(cid: CombatantID) = ApplyDamageEvent(cid, 1000)

  /**
   * Helper event to allow hacks to the event source. This should be used only in test. It allows for a generic
   * transformation of state via a event.
   */
  protected case class ForceChangeEvent(mod: CombatState => CombatState) extends CombatStateEvent {
    def transition(iState: CombatState): CombatState = mod(iState)
  }

}