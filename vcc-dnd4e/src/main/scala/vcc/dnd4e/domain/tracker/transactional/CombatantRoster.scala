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
//$Id$
package vcc.dnd4e.domain.tracker.transactional

import vcc.controller.transaction.{Transaction, Undoable}
import vcc.dnd4e.domain.tracker.common.RosterChange
import vcc.dnd4e.tracker.common.{CombatantID, RosterCombatantFactory, CombatantRosterDefinition, Roster}
import vcc.dnd4e.tracker.common.{CombatantEntity, CombatantType}

/**
 * Contains a transactional view of all the combatants currently in the combat.
 */
class CombatantRoster() extends RosterCombatantFactory[Combatant] {

  //This is a hack since we need the transaction for replaceCombatant
  implicit var trans: Transaction = null
  /**
   *  Roster internal map
   */
  def replaceDefinition(combatant: Combatant, newDefinition: CombatantRosterDefinition): Combatant = {
    combatant.setDefinition(newDefinition)
    combatant
  }

  def createCombatant(definition: CombatantRosterDefinition): Combatant = {
    new Combatant(definition)
  }

  private val _roster = new Undoable[Roster[Combatant]](Roster(this, Map()),
    (self, oldV) => if (self.value.entries.keySet == oldV.entries.keySet) {
      Nil
    } else {
      List(RosterChange(Map(self.value.entries.map(me => me._1 -> me._2.aspectSet()).toSeq: _*)))
    }
  )

  /**
   * Add a combatant to the roster. CombatantID will be generated if not provided.
   * @param cid CombatantID for the new combatant. If null is provided an Id will be generated.
   * @param alias A name alias for the combatant
   * @param entity The Entity to be used as base for the entity
   */
  def addCombatant(cid: CombatantID, alias: String, entity: CombatantEntity)(implicit transaction: Transaction) {
    trans = transaction
    _roster.value_=(_roster.value.addCombatant(if (cid == null) None else Some(cid), alias, entity))(trans)
    trans = null
  }

  /**
   * Return a combatant
   * @param cid Id to be found
   * @return The combatant if found, otherwise will throw a NoSuchElementException
   */
  def combatant(cid: CombatantID): Combatant = _roster.value.combatant(cid)

  /**
   * Determine is cid is defined as a Combatant in the Roster
   * @param cid ID to be search the roster for
   * @return True if the Roster contains a Combatant for that ID
   */
  def isDefinedAt(cid: CombatantID) = _roster.value.isDefinedAt(cid)

  /**
   * Returns all the CombatantID in the roster
   */
  def allCombatantIDs: List[CombatantID] = _roster.value.allCombatantIDs

  /**
   * Clear roster of combatants
   * @param all If true will clear Play Combatant and NPC (non-player combatants), when fall only NPC are cleared.
   */
  def clear(all: Boolean)(implicit trans: Transaction) {
    _roster.value = _roster.value.clear(re => all || re.definition.entity.ctype != CombatantType.Character)
  }
}