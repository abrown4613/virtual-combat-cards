/**
 *  Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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

import vcc.util.DiceBag
import vcc.infra.util.{ArrayRoundRobin, ReorderedListBuilderCompare, ReorderedListBuilder}
import vcc.controller.transaction.{UndoableWithCallback, Undoable, Transaction}
import vcc.dnd4e.domain.tracker.common._

/**
 * InitiativeOrder keeps the information about acting Combatants.
 * It provides transactional control over initiative rolled, reorders, and the rotation of
 * the RoundRobin.
 */
class InitiativeOrder {
  private object initiativeResultComparator extends ReorderedListBuilderCompare[InitiativeResult] {
    def isBefore(a: InitiativeResult, b: InitiativeResult): Boolean = {
      if (a.compare(b) == 0) DiceBag.flipCoin()
      else a > b
    }
  }

  private val robinHead = new Undoable[InitiativeOrderID](null, x => InitiativeOrderFirstChange(x.value))
  private val initBaseOrder = new Undoable[List[InitiativeResult]](Nil, null)
  private val trackers = new Undoable(Map.empty[InitiativeOrderID, Undoable[InitiativeTracker]], null)
  private val robin = new ArrayRoundRobin[InitiativeOrderID](null, Nil)
  private val reorders = new Undoable[List[(InitiativeOrderID, InitiativeOrderID)]](Nil, null)
  private val initOrder = new Undoable[List[InitiativeOrderID]](Nil, x => InitiativeOrderChange(x.value)) with UndoableWithCallback[List[InitiativeOrderID]] {
    def restoreCallback(oldList: List[InitiativeOrderID]) {
      robin.setRobin(if (oldList.isEmpty) null else robinHead.value, oldList)
    }
  }

  /**
   * Return the InitiativeTracker for InitiativeOrderID
   * @param ioid The ID being searched
   * @throw NoSuchElementException If the entry can be found
   */
  def initiativeTrackerFor(ioid: InitiativeOrderID): InitiativeTracker = {
    if (trackers.value.isDefinedAt(ioid)) trackers.value(ioid).value
    else throw new NoSuchElementException("InitiativeTracker for " + ioid + " not found")
  }

  /**
   * Informs if the ioid is pointing to a valid tracker, in other words if it has a position in the order.
   * @param ioid InitiativeOrderID that point to the InitiativeTracker
   * @return True if ioid  points to a InitiativeTracker
   */
  def isDefinedAt(ioid: InitiativeOrderID) = trackers.value.isDefinedAt(ioid)

  /**
   * Updates a initiative tracker for a given InitiativeOrderId.
   * @param ioid InitiativeOrder order it
   * @param newValue The new initaitive tracker
   * @throw NoSuchElementException if there is no such tracker
   */
  def updateInitiativeTrackerFor(ioid: InitiativeOrderID, newValue: InitiativeTracker)(implicit trans: Transaction) {
    trackers.value(ioid).value = newValue
  }

  /**
   *  Move head of the round robin one position down
   */
  def rotate()(implicit trans: Transaction) {
    if (robinHead.value == null) throw new IllegalStateException("Have not issued startCombat()")
    robin.advance()
    robinHead.value = robin.headOption.get
  }

  /**
   * This changes the behaviour of the InitiativeOrder  to define the first in the order and to keep it during changes.
   * Will do nothing on further calls.
   */
  def startCombat()(implicit trans: Transaction) {
    if (robinHead.value == null) {
      robinHead.value = initOrder.value(0)
    }
  }

  /**
   * Move someone before the other in the initiative order. This will not affect the
   * current head. Must be done after startCombat.
   * @param who The Combatant to be move
   * @param whom Whom to place it before in the order
   */
  def moveBefore(who: InitiativeOrderID, whom: InitiativeOrderID)(implicit trans: Transaction) {
    if (robinHead.value == null) throw new IllegalStateException("Not in combat")
    val ioBuilder = new ReorderedListBuilder(initBaseOrder.value, reorders.value, initiativeResultComparator)
    ioBuilder.addReorder(who, whom)
    initOrder.value = ioBuilder.reorderedList()
    robin.setRobin(robinHead.value, initOrder.value)
    reorders.value = ioBuilder.reorders()
  }

  /**
   * Remove all information from a Combatant, must be done prior to <code>startCombat</code>
   * @param comb The combatant to remove from the order
   */
  def removeCombatant(comb: CombatantID)(implicit trans: Transaction) {
    if (robinHead.value != null) throw new IllegalStateException("Can't remove after combat start")

    val toRemove = initOrder.value.filter(e => e.combId == comb)
    initOrder.value = initOrder.value -- toRemove
    toRemove.foreach {o => updateInitiativeTrackerFor(o, null)}
    trackers.value = trackers.value -- toRemove
    initBaseOrder.value = initBaseOrder.value.filter(e => e.uniqueId.combId != comb)
  }

  /**
   * Will add a Definition to the order if its on in the order. This will regenerate
   * the order and also add the base InitiativeTracker
   */
  def setInitiative(iDef: InitiativeDefinition)(implicit trans: Transaction) {
    val initRes = iDef.toResult()
    var itMap = trackers.value
    val ioBuilder = new ReorderedListBuilder(initBaseOrder.value, reorders.value, initiativeResultComparator)
    for (res <- initRes) {
      val it = new Undoable[InitiativeTracker](null, u => InitiativeTrackerChange(res.uniqueId, u.value))
      itMap = itMap + (res.uniqueId -> it)
      it.value = InitiativeTracker.initialTracker(res.uniqueId)
      ioBuilder.addEntry(res)
    }

    //Need to make sure we preserve dice rolls for next call
    val bl = ioBuilder.baseList()
    initBaseOrder.value = (bl.length to 1 by -1).toList.zip(bl).map(p => p._2.setTieBreak(p._1))
    initOrder.value = ioBuilder.reorderedList()
    trackers.value = itMap
    robin.setRobin(robinHead.value, initOrder.value)
  }

  /**
   * Removes all entries from the InitiativeOrder transitionally. This also doubles as an EndCombat
   * since the order is undone.
   */
  def clearOrder()(implicit trans: Transaction) {
    initOrder.value = Nil
    if (robinHead.value != null) robinHead.value = null
    trackers.value.keys.foreach(updateInitiativeTrackerFor(_, null))
    trackers.value = Map()
    initBaseOrder.value = Nil
    reorders.value = Nil
  }

  /**
   * Returns the IDs in the Order
   */
  def getIDsInOrder(): List[InitiativeOrderID] = initOrder.value
}