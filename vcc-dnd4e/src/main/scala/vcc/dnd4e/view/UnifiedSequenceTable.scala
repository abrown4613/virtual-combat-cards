/*
 *
 *  Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.view

import vcc.dnd4e.domain.tracker.common.CombatStateView
import vcc.dnd4e.tracker.common._

/**
 * Provides a single linear and random access table with all combatants.
 * @param elements The list of UnifiedCombatant in the order
 * @param state The state that was used to build the sequence
 */
class UnifiedSequenceTable(val elements: Array[UnifiedCombatant], val state: CombatStateView) {

  /**
   * Get UnifiedCombatant at position idx
   * @param idx Index of the UnifiedCombatant on the table
   */
  def apply(idx: Int): UnifiedCombatant = elements(idx)

  /**
   * Get a combatant based on its orderId and combId. This is the proper way to get combatants that are not in order.
   */
  def apply(combId: CombatantID, orderId: InitiativeOrderID): UnifiedCombatant = {
    val m = elements.find(e => e.combId == combId && orderId == e.orderId)
    if (m.isDefined) m.get else null
  }

  /**
   * Get a combatant with InitiativeOrderID (will infer CombatantID from the order ID)
   */
  def apply(orderId: InitiativeOrderID): UnifiedCombatant = {
    if (orderId != null) {
      val m = elements.find(e => e.matches(orderId))
      if (m.isDefined) m.get else null
    } else null
  }

  /**
   * Get a combatant option, based on an option on UnifiedCombatantID.
   */
  def combatantOption(uid: Option[UnifiedCombatantID]): Option[UnifiedCombatant] =
    if (uid.isDefined) elements.find(e => e.matches(uid))
    else None

  /**
   * Return the first UnifiedCombatant in the robin.
   */
  def orderFirst: Option[UnifiedCombatant] = state.nextUp.map(orderId => this(orderId.combId, orderId))

  /**
   * Return the UnifiedCombatantID of the first combatant in the robin.
   */
  def orderFirstId: Option[UnifiedCombatantID] = state.nextUp.map(orderId => UnifiedCombatantID(orderId.combId, orderId))

  def indexOf(id: UnifiedCombatantID): Option[Int] = {
    val index = elements.indexWhere(_.matches(id))
    if (index < 0) None else Some(index)
  }
}

/**
 * Companion object for UnifiedSequenceTable with is used to build the sequences
 */
object UnifiedSequenceTable {


  private trait InitiativeOrderViewBuilder {
    def buildOrder(combatState: CombatStateView): Seq[InitiativeOrderID]
  }

  private object DirectInitiativeOrderViewBuilder extends InitiativeOrderViewBuilder {
    def buildOrder(combatState: CombatStateView): Seq[InitiativeOrderID] = {
      combatState.getInitiativeOrder
    }
  }

  private object RobinHeadFirstInitiativeOrderViewBuilder extends InitiativeOrderViewBuilder {
    def buildOrder(combatState: CombatStateView): Seq[InitiativeOrderID] = {
      val order = combatState.getInitiativeOrder
      val idx: Int = if (combatState.nextUp.isDefined && order.contains(combatState.nextUp.get)) order.indexOf(combatState.nextUp.get) else 0
      order.drop(idx) ++ order.take(idx)
    }
  }

  class Builder {
    private var orderBuilder: InitiativeOrderViewBuilder = RobinHeadFirstInitiativeOrderViewBuilder
    private var filterDead = false

    def showDead() {
      filterDead = false
    }

    def hideDead() {
      filterDead = true
    }

    def useRobinOrder() {
      orderBuilder = RobinHeadFirstInitiativeOrderViewBuilder
    }

    def useDirectOrder() {
      orderBuilder = DirectInitiativeOrderViewBuilder
    }

    def build(combatState: CombatStateView): UnifiedSequenceTable = {
      def isAliveOrActing(combatant: UnifiedCombatant): Boolean = {
        (combatant.health.status != HealthStatus.Dead) ||
          (combatant.initiative != null && combatant.initiative.state == InitiativeState.Acting)
      }

      var elements = makeList(combatState)
      if (filterDead)
        elements = elements.filter(isAliveOrActing)

      new UnifiedSequenceTable(elements, combatState)
    }

    private def makeList(combatState: CombatStateView): Array[UnifiedCombatant] = {
      def makeUnifiedCombatant(combId: CombatantID, orderId: InitiativeOrderID): UnifiedCombatant = {
        val initiativeTracker = if (orderId != null) combatState.initiativeTrackerFromID(orderId) else null
        new UnifiedCombatant(combId, initiativeTracker, combatState.combatantViewFromID(combId))
      }

      val order = orderBuilder.buildOrder(combatState).map(e => makeUnifiedCombatant(e.combId, e))
      val reserve = combatantNotInOrderSortedById(combatState).map(e => makeUnifiedCombatant(e, null))
      (order ++ reserve).toArray
    }

    private def combatantNotInOrderSortedById(combatState: CombatStateView): Seq[CombatantID] = {
      val notInOrder = Set[CombatantID]((combatState.allCombatantIDs filterNot (combatState.getInitiativeOrder.map(_.combId) contains)): _*)
      notInOrder.toList.sortWith((a: CombatantID, b: CombatantID) => a.id < b.id)
    }
  }

}