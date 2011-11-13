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
package vcc.dnd4e.domain.tracker.snapshot

import vcc.controller.SnapshotBuilder
import vcc.controller.transaction.ChangeNotification
import vcc.dnd4e.domain.tracker.common._
import java.lang.Throwable
import org.slf4j.LoggerFactory
import vcc.util.swing.AbortApplication


/**
 * Helper mixin to cause abort on a failure.
 */
trait SnapshotBuilderAborter[T] {
  self: SnapshotBuilder[T] =>

  def handleFailure(e: Throwable, changes: List[ChangeNotification]) {
    val logger = LoggerFactory.getLogger("domain")
    logger.error("Failed to handle changes: {}", changes)
    logger.error("Causing exception: " + e.getMessage, e)
    AbortApplication(this, "Internal error while updating state", e)
  }

}

/**
 * Represents a state along with changes.
 * @param state CombatState
 * @param changes Changes since last begin/end cycle
 */
case class CombatStateWithChanges(state: SnapshotCombatState, changes: StateChange)

/**
 * This SnapshotBuilder will wrap a CombatStateSnapshotBuilder and keep track of changes in the last interaction of
 * processing changes.
 * @param subBuilder A builder that will handle CombatState changes
 */
class CombatChangeAndStateSnapshotBuilder(subBuilder: CombatStateSnapshotBuilder)
        extends SnapshotBuilder[CombatStateWithChanges] with SnapshotBuilderAborter[CombatStateWithChanges] {
  private var changes: StateChange = null

  def this() = this (new CombatStateSnapshotBuilder())

  def processChange(change: ChangeNotification) {
    if (!change.isInstanceOf[CombatStateChange]) throw new IllegalArgumentException("Can't handle: " + change)
    change.asInstanceOf[CombatStateChange] match {
      case InitiativeOrderChange(newOrder) =>
        changes.add(StateChange.combat.Order)
        newOrder.foreach(e => changes.add(e.orderID.combId, StateChange.combatant.Initiative))

      case InitiativeTrackerChange(it) => changes.add(it.orderID.combId, StateChange.combatant.Initiative)

      case InitiativeOrderFirstChange(who) => changes.add(StateChange.combat.OrderFirst)

      case RosterChange(newRoster) =>
        newRoster.foreach(e => e._2.foreach(aspect => changes.add(e._1, StateChange.changeFromAspect(aspect))))
        changes.add(StateChange.combat.Roster)

      case CombatantChange(who, what) => changes.add(who, StateChange.changeFromAspect(what))

      case CombatMetaDataChange(fight, comment) => changes.add(StateChange.combat.MetaData)
    }
    subBuilder.processChange(change)
  }

  def getSnapshot(): CombatStateWithChanges = CombatStateWithChanges(subBuilder.getSnapshot(), changes)

  def endChanges() {
    subBuilder.endChanges()
  }

  def beginChanges() = {
    subBuilder.beginChanges()
    changes = new StateChange()
  }
}