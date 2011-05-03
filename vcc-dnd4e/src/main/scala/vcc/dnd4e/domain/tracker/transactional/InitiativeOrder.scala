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

import vcc.dnd4e.tracker.common.{InitiativeOrder => ImmutableInitiativeOrder}
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.domain.tracker.common.{InitiativeTrackerChange, InitiativeOrderChange, InitiativeOrderFirstChange}
import vcc.controller.transaction.{ChangeNotification, Undoable, Transaction}

/**
 * InitiativeOrder keeps the information about acting Combatants.
 * It provides transactional control over initiative rolled, reorders, and the rotation of
 * the RoundRobin.
 */
class InitiativeOrder {

  private val _order = new Undoable[ImmutableInitiativeOrder](ImmutableInitiativeOrder.empty(), (u, v) => {
    val changes: List[ChangeNotification] =
      (if (v.nextUp != u.value.nextUp)
        List(InitiativeOrderFirstChange(if (u.value.nextUp.isDefined) u.value.nextUp.get else null))
      else Nil) :::
        (if (v.order != u.value.order) List(InitiativeOrderChange(u.value.order.map(u.value.tracker(_)))) else Nil) :::
        (if (v.tracker != u.value.tracker) {
          var c: List[ChangeNotification] = Nil
          u.value.tracker.foreach(p =>
            if ((v.tracker.isDefinedAt(p._1) && v.tracker(p._1) != p._2))
              c = InitiativeTrackerChange(p._2) :: c
          )
          c.reverse
        } else Nil) :::
        Nil
    changes
  })

  /**
   * Return the InitiativeTracker for InitiativeOrderID
   * @param ioid The ID being searched
   * @throw NoSuchElementException If the entry can be found
   */
  def initiativeTrackerFor(ioid: InitiativeOrderID): InitiativeTracker = {
    if (_order.value.tracker.isDefinedAt(ioid)) _order.value.tracker(ioid)
    else throw new NoSuchElementException("InitiativeTracker for " + ioid + " not found")
  }

  /**
   * Informs if the ioid is pointing to a valid tracker, in other words if it has a position in the order.
   * @param ioid InitiativeOrderID that point to the InitiativeTracker
   * @return True if ioid  points to a InitiativeTracker
   */
  def isDefinedAt(ioid: InitiativeOrderID) = _order.value.tracker.isDefinedAt(ioid)

  /**
   * Returns the InitiativeTracker for the head InitiativeOrderID.
   */
  def robinHeadInitiativeTracker(): InitiativeTracker =
    if (_order.value.nextUp.isDefined) initiativeTrackerFor(_order.value.nextUp.get)
    else throw new NoSuchElementException("There is no head for the order yet")


  /**
   * Updates a initiative tracker for a given InitiativeOrderId.
   * @param ioid InitiativeOrder order it
   * @param newValue The new initiative tracker
   * @throw NoSuchElementException if there is no such tracker
   */
  def updateInitiativeTrackerFor(ioid: InitiativeOrderID, newValue: InitiativeTracker)(implicit trans: Transaction) {
    if (ioid != newValue.orderID) throw new IllegalArgumentException("InitiativeTracker orderId does not match parameter orderID")
    _order.value = _order.value.updateTracker(newValue)
  }

  /**
   *  Move head of the round robin one position down
   */
  def rotate()(implicit trans: Transaction) {
    _order.value = _order.value.rotate()
  }

  /**
   * This changes the behaviour of the InitiativeOrder  to define the first in the order and to keep it during changes.
   * Will do nothing on further calls.
   */
  def startCombat()(implicit trans: Transaction) {
    _order.value = _order.value.startCombat()
  }

  /**
   * Move someone before the other in the initiative order. This will not affect the
   * current head. Must be done after startCombat.
   * @param who The Combatant to be move
   * @param whom Whom to place it before in the order
   */
  def moveBefore(who: InitiativeOrderID, whom: InitiativeOrderID)(implicit trans: Transaction) {
    _order.value = _order.value.moveBefore(who, whom)
  }

  /**
   * Remove all information from a Combatant, must be done prior to <code>startCombat</code>
   * @param comb The combatant to remove from the order
   */
  def removeCombatant(comb: CombatantID)(implicit trans: Transaction) {
    _order.value = _order.value.removeCombatant(comb)
  }

  /**
   * Will add a Definition to the order if its on in the order. This will regenerate
   * the order and also add the base InitiativeTracker
   */
  def setInitiative(iDef: InitiativeDefinition)(implicit trans: Transaction) {
    _order.value = _order.value.setInitiative(iDef)
  }

  /**
   * Removes all entries from the InitiativeOrder transitionally. This also doubles as an EndCombat
   * since the order is undone.
   */
  def clearOrder()(implicit trans: Transaction) {
    _order.value = ImmutableInitiativeOrder.empty()
  }

  /**
   * Set the robin head, this will advance to a new head in the Robin. Used for MoveUp actions.
   * @param orderId The InitiativeOrderID that should be the new head of the Robin
   */
  def setRobinHead(orderId: InitiativeOrderID)(implicit trans: Transaction) {
    _order.value = _order.value.setNextUp(orderId)
  }

  /**
   * Returns the IDs in the Order
   */
  def getIDsInOrder: List[InitiativeOrderID] = _order.value.order

  /**
   * Check if the combatant is in the Initiative Order.
   * @param cmb The Combatant to search in the order.
   */
  def isInOrder(cmb: CombatantID): Boolean = _order.value.order.exists(x => x.combId == cmb)

  /**
   * Will eliminate all Trackers that are  not in list of ID. Must not be in combat.
   * @param combatants List of combatant to be kept
   */
  def syncOrderToRoster(combatants: List[CombatantID])(implicit trans: Transaction) {
    val missing = getIDsInOrder.map(id => id.combId).toSet -- combatants
    missing.foreach {
      id => removeCombatant(id)
    }
  }
}
