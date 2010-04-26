/**
 * Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.common

import vcc.controller.transaction.ChangeNotification
import vcc.dnd4e.model.CombatantEntity

/**
 * Some part of the Combatant information
 */
trait CombatantAspect

/**
 * This is a simple wrapper for a text, to allow us to identify the String as the comment on the combatant.
 */
case class CombatantComment(text: String) extends CombatantAspect

/**
 *  Defines the major CombatantRoster data for a tracker.transactional.Combatant, it is also used for views.
 */
case class CombatantRosterDefinition(cid: CombatantID, alias: String, entity: CombatantEntity) extends CombatantAspect

//Change Notifications
abstract sealed class CombatStateChange extends ChangeNotification


/**
 * Indicates the change of a single CombatantAspect 
 */
case class CombatantChange(comb: CombatantID, obj: CombatantAspect) extends CombatStateChange

/**
 * This class is used to notify that a change has happened in an InitiativeTracker. If the tracker is <code>null</code>
 * there is no longer a meaningful value associated to it.
 * @param ioid InitiativeOrderID of the tracker owner
 * @param initTracker New value of the initiative tracker, null indicates that this InitiativeOrderID  is no longer being
 * tracked.
 */
case class InitiativeTrackerChange(initTracker: InitiativeTracker) extends CombatStateChange

/**
 * Indicates a change in the initiative order, this is result of a reorder, removing or adding a combatant.
 * @param order New order, Nil indicates that we are no longer tracking order
 */
case class InitiativeOrderChange(order: List[InitiativeTracker]) extends CombatStateChange {
  override def toString(): String = "InitiativeOrderChange(" + order.map(x => x.orderID.toLabelString).mkString(", ") + ")"
}

/**
 * Indicates the new head of the initiative order, this is an indication that we have started combat or rotated the
 * current order.
 * @param ioid New head of the initiative order, <code>null</code> indicates that we are no longer in combat.
 */
case class InitiativeOrderFirstChange(ioid: InitiativeOrderID) extends CombatStateChange


/**
 * Indicates that the CombatantRoster has changed and that may include new entities. All
 * aspects are sent too.
 * @param combatants A map of CombatantID to the aspects of that combatants 
 */
case class RosterChange(combatants: Map[CombatantID, Set[CombatantAspect]]) extends CombatStateChange

/**
 * Indicates a changes in the CombatMetaData.
 * @param inBattle Is the tracker in battle or not?
 * @param comment General not about the battle
 */
case class CombatMetaDataChange(inBattle: Boolean, comment: String) extends CombatStateChange