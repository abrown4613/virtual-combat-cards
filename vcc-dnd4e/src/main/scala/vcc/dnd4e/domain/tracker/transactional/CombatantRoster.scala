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

import vcc.model.IDGenerator
import vcc.controller.transaction.{UndoableWithCallback, Transaction, Undoable}
import vcc.dnd4e.tracker.common.{CombatantRosterDefinition}
import vcc.dnd4e.tracker.common.CombatantType
import vcc.dnd4e.tracker.common.{CombatantEntity, CombatantID}
import vcc.dnd4e.domain.tracker.common.RosterChange

/**
 * Contains a transactional view of all the combatants currently in the combat.
 */
class CombatantRoster(idGenerator: IDGenerator) {
  def this() = this (new IDGenerator(1, 50))

  /**
   *  Roster internal map
   */
  private val _roster = new Undoable[Map[CombatantID, Combatant]](
    Map.empty[CombatantID, Combatant],
    uv => {
      RosterChange(Map(uv.value.map(me => me._1 -> me._2.aspectSet()).toSeq: _*))
    }
  ) with UndoableWithCallback[Map[CombatantID, Combatant]] {

    // Find IDs that are gone and return them to ID Generator
    def restoreCallback(valueToRestore: Map[CombatantID, Combatant]) {
      val gone = (this.value -- valueToRestore.keys).map(me => me._1.toSymbol)
      gone.foreach(id => idGenerator.returnToPool(id))
      val back = (valueToRestore -- this.value.keys).map(me => me._1.toSymbol)
      back.foreach(id => if (idGenerator.contains(id)) idGenerator.removeFromPool(id))
    }
  }

  /**
   * Add a combatant to the roster. CombatantID will be generated if not provided.
   * @param cid CombatantID for the new combatant. If null is provided an Id will be generated.
   * @param alias A name alias for the combatant
   * @param entity The Entity to be used as base for the entity
   */
  def addCombatant(cid: CombatantID, alias: String, entity: CombatantEntity)(implicit trans: Transaction) {
    // Generate ID if necessary
    val combId: CombatantID = if (cid == null) {
      val id = idGenerator.first()
      CombatantID(id.name)
    } else {
      val cidSymbol = Symbol(cid.id)
      if (idGenerator.contains(cidSymbol)) idGenerator.removeFromPool(cidSymbol)
      cid
    }
    val definition = CombatantRosterDefinition(combId, alias, entity)
    if (_roster.value.isDefinedAt(combId)) {
      // We need to update this dude
      combatant(combId).setDefinition(definition)
    } else {
      _roster.value = _roster.value + (definition.cid -> new Combatant(definition))
    }
  }

  /**
   * Return a combatant
   * @param cid Id to be found
   * @return The combatant if found, otherwise will throw a NoSuchElementException
   */
  def combatant(cid: CombatantID): Combatant = _roster.value(cid)

  /**
   * Determine is cid is defined as a Combatant in the Roster
   * @param cid ID to be search the roster for
   * @return True if the Roster contains a Combatant for that ID
   */
  def isDefinedAt(cid: CombatantID) = _roster.value.isDefinedAt(cid)

  /**
   * Returns all the CombatantID in the roster
   */
  def allCombatantIDs: List[CombatantID] = _roster.value.keys.toList

  /**
   * Clear roster of combatants
   * @param all If true will clear Play Combatant and NPC (non-player combatants), when fall only NPC are cleared.
   */
  def clear(all: Boolean)(implicit trans: Transaction) {
    if (all) {
      _roster.value.keys.map(e => e.toSymbol).foreach(s => idGenerator.returnToPool(s))
      _roster.value = Map()
    } else {
      _roster.value = _roster.value.filter(re => {
        if (re._2.definition.entity.ctype == CombatantType.Character) {
          true
        } else {
          //Return ids we will filter to the pool
          idGenerator.returnToPool(re._1.toSymbol)
          false
        }
      })
    }
  }
}