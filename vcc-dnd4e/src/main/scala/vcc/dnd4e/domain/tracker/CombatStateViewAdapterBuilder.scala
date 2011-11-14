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
package vcc.dnd4e.domain.tracker

import common.{CombatStateChangeNotification, CombatantStateView, CombatStateView}
import vcc.controller.SnapshotBuilder
import vcc.controller.transaction.ChangeNotification
import vcc.dnd4e.tracker.common._

class CombatStateViewAdapterBuilder() extends SnapshotBuilder[CombatStateView] {

  private class CombatantStateViewAdapter(combatant: Combatant) extends CombatantStateView {
    def healthTracker: HealthTracker = combatant.health

    def effects: EffectList = combatant.effects

    def definition: CombatantRosterDefinition = combatant.definition

    def comment: String = combatant.comment
  }

  private class CombatStateViewAdapter(combatState: CombatState) extends CombatStateView {
    private val combatantViewMap = combatState.roster.entries.map(e => (e._1, new CombatantStateViewAdapter(e._2)))

    def combatantViewFromID(id: CombatantID): CombatantStateView = combatantViewFromID(id)

    def allCombatantIDs: List[CombatantID] = combatState.roster.allCombatantIDs

    def initiativeTrackerFromID(orderId: InitiativeOrderID): InitiativeTracker = combatState.order.tracker(orderId)

    def getInitiativeOrder: List[InitiativeOrderID] = combatState.order.sequence

    def isCombatStarted: Boolean = combatState.isCombatStarted

    def combatComment: String = combatState.comment.getOrElse(null)

    def nextUp: Option[InitiativeOrderID] = combatState.order.nextUp
  }

  private var combatState: CombatState = CombatState.empty

  def beginChanges() {}

  def processChange(change: ChangeNotification) {
    change match {
      case CombatStateChangeNotification(newState) => combatState = newState
      case s =>
        println("***Ignoring : " + s)
    }
  }

  def endChanges() {}

  def getSnapshot(): CombatStateView = {
    new CombatStateViewAdapter(combatState)
  }

  def handleFailure(e: Throwable, change: List[ChangeNotification]) {}
}