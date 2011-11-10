/**
 *  Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
//$Id4
package vcc.dnd4e.domain.tracker.transactional

import vcc.dnd4e.tracker.common._

//import vcc.dnd4e.tracker.common.{CombatState => ImmutableCombatState, Combatant => ImmCombatant, InitiativeTracker => ImmInitiativeTracker}

import vcc.controller.transaction.Undoable
import vcc.dnd4e.tracker.dispatcher.CombatStateChangePublisher

/**
 * CombatState provides an aggregate of the CombatantRoster, InitiativeOrder, and CombatMetaData. It also provides
 * some helper extractors for the tracker logic.
 */
class CombatContext() {

  val iState = new Undoable[CombatState](CombatState.empty, (us, old) => CombatStateChangePublisher.publish(old, us.value))

  /**
   * This is an extractor to get the InitiativeTracker from a InitiativeOrderID
   */
  object initTrackerFromID {
    def unapply(orderID: InitiativeOrderID): Option[InitiativeTracker] = iState.value.order.tracker.get(orderID)
  }

  /**
   * This is an extractor to get the Combatant from a CombatantID
   */
  object combatantFromID {
    def unapply(combID: CombatantID): Option[Combatant] = iState.value.roster.entries.get(combID)
  }

  /**
   * This is an extractor to get the Combatant from a EffectID
   */
  object combatantFromEffectID {
    def unapply(effectId: EffectID): Option[Combatant] = iState.value.roster.entries.get(effectId.combId)
  }

  def isCombatStarted: Boolean = iState.value.isCombatStarted

  /**
   * Helper method to return combatant based on id.
   */
  def combatantFromID(cid: CombatantID): Combatant = iState.value.roster.combatant(cid)

  /**
   * Enumerate all Effect in the combat.
   */
  def allEffects: Seq[Effect] = {
    iState.value.roster.entries.flatMap(p => p._2.effects.effects).toSeq
  }
}