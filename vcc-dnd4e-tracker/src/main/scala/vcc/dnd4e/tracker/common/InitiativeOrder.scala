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
package vcc.dnd4e.tracker.common

import vcc.infra.util.{ReorderedListBuilderCompare, ReorderedListBuilder}
import vcc.util.DiceBag

case class InitiativeOrder private[common](tracker: Map[InitiativeOrderID, InitiativeTracker],
                                           baseList: List[InitiativeResult],
                                           reorderList: List[(InitiativeOrderID, InitiativeOrderID)],
                                           order: List[InitiativeOrderID],
                                           nextUp: Option[InitiativeOrderID]) {

  /**
   * Clear all order information.
   */
  def endCombat() = InitiativeOrder.empty()

  /**
   * This changes the behaviour of the InitiativeOrder  to define the first in the order and to keep it during changes.
   * Will do nothing on further calls.
   */
  def startCombat() = {
    if (order.isEmpty)
      throw new IllegalStateException("No combatant has initiative")
    copy(
      nextUp = Some(order.head)
    )
  }

  /**
   * Rotate robin head to next position in order.
   */
  def rotate(): InitiativeOrder = {
    if (!nextUp.isDefined)
      throw new IllegalStateException("Rotate not allowed before start of combat")
    val idx = order.indexOf(nextUp.get) + 1
    if (idx < order.length) copy(nextUp = Some(order(idx)))
    else copy(nextUp = Some(order.head))
  }

  /**
   * Will add a Definition to the order if its on in the order. This will regenerate
   * the order and also add the base InitiativeTracker
   */
  def setInitiative(iDef: InitiativeDefinition): InitiativeOrder = {
    val initRes = iDef.toResult()
    var itMap = Map.empty[InitiativeOrderID, InitiativeTracker]
    val ioBuilder = new ReorderedListBuilder(baseList, reorderList, InitiativeOrder.initiativeResultComparator)
    for (res <- initRes) {
      val it = InitiativeTracker.initialTracker(res.uniqueId, res.result)
      itMap = itMap + (res.uniqueId -> it)
      ioBuilder.addEntry(res)
    }

    //Need to make sure we preserve dice rolls for next call
    val bl = ioBuilder.baseList()
    this.copy(
      baseList = (bl.length to 1 by -1).toList.zip(bl).map(p => p._2.setTieBreak(p._1)),
      order = ioBuilder.reorderedList(),
      tracker = tracker ++ itMap
    )
  }

  /**
   * Remove all information from a Combatant, must done while not in order
   * @param comb The combatant to remove from the order
   *
   */
  def removeCombatant(comb: CombatantID) = {
    if (nextUp.isDefined) throw new IllegalStateException("Can't remove after combat start")
    this.copy(
      tracker = tracker.filterKeys(k => k.combId != comb),
      order = order.filter(k => k.combId != comb),
      baseList = baseList.filter(k => k.uniqueId.combId != comb)
    )
  }

  /**
   * Move someone before the other in the initiative order. This will not affect the
   * current head. Must be done after startCombat.
   * @param who The Combatant to be move
   * @param whom Whom to place it before in the order
   */
  def moveBefore(who: InitiativeOrderID, whom: InitiativeOrderID) = {
    if (!nextUp.isDefined) throw new IllegalStateException("Can't move if combat not started")
    val ioBuilder = new ReorderedListBuilder(baseList, reorderList, InitiativeOrder.initiativeResultComparator)
    ioBuilder.addReorder(who, whom)
    this.copy(
      order = ioBuilder.reorderedList(),
      reorderList = ioBuilder.reorders()
    )
  }

  /**
   * Changes an InitiativeTracker in the current tracker list. If the tracker does not exist will throw and exceptions.
   * @param it InitiativeTracker to be updated.
   * @return InitiativeOrder with new tracker value
   * @throw NoSuchElement if the order does not have a tracker identified by it.orderId
   */
  def updateTracker(it: InitiativeTracker): InitiativeOrder = {
    if (!tracker.isDefinedAt(it.orderID))
      throw new NoSuchElementException("Tracker for '" + it.orderID + "' is not defined")
    this.copy(tracker = tracker.updated(it.orderID, it))
  }

  /**
   * Set the robin head, this will advance to a new head in the Robin. Used for MoveUp actions.
   * @param orderId The InitiativeOrderID that should be the new head of the Robin
   * @throw IllegalStateException if combat is not started or NoSuchElementException if orderId is not part of
   */
  def setNextUp(orderId: InitiativeOrderID): InitiativeOrder = {
    if (!nextUp.isDefined) throw new IllegalStateException("Combat not started")
    if (!order.contains(orderId)) throw new NoSuchElementException("Not in order " + orderId)
    else {
      this.copy(nextUp = Some(orderId))
    }
  }

  /**
   * Check the validity and cross references between internal data structures. An order is valid it the following is
   * true:
   * <ul>
   * <li>Each InitiativeOrderID in the order has a corresponding InitiativeTracker</li>
   * <li>The nextUp is either None (not in combat) or Some(orderId) where orderId is in the initiative order
   * <li>Reorder only include InitiativeOrderID in the order
   * <li>All InitiativeTracker in tracker have an element in the order
   * </ul>
   */
  def isValid: Boolean = {
    (this.order.toSet == this.tracker.keys.toSet) &&
      (if (nextUp.isDefined) order.contains(nextUp.get) else true) &&
      (!reorderList.exists(p => !order.contains(p._1) || !order.contains(p._2)))
  }
}

object InitiativeOrder {

  private[common] object initiativeResultComparator extends ReorderedListBuilderCompare[InitiativeResult] {
    def isBefore(a: InitiativeResult, b: InitiativeResult): Boolean = {
      if (a.compare(b) == 0) DiceBag.flipCoin()
      else a > b
    }
  }

  def empty() = InitiativeOrder(Map.empty, Nil, Nil, Nil, None)
}
