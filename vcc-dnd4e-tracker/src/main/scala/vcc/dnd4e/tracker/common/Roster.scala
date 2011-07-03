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

/**
 * This is a Roster companion interface that provide a way to implement to services needed by the Roster, creating and
 * replacing a Combatant entity.
 */
trait RosterCombatantFactory[T] {
  /**
   * Should create a blank combatant of type T based on the CombatantRosterDefinition provided
   * @param definition Template for the new combatant
   * @return A valid combatant of type T
   */
  def createCombatant(definition: CombatantRosterDefinition): T

  /**
   * Called when an existing combatant must be redefined, which happens if it is already in the Roster.
   * Implementation should preserve definition independent parts of the combatant.
   * @param combatant Current value of the combatant
   * @param newDefinition New definition for the combatant
   * @return A modified copy of combatant
   */
  def replaceDefinition(combatant: T, newDefinition: CombatantRosterDefinition): T
}

/**
 * Roster is essentially a map with contain combatants. It provides the ability to generate unique CombatantID if needed
 * and to replace definitions if the adding of a combatant is replacing a previous one.
 * @param factory This interface provides methods to create and replace combatants based on the definition
 * @param entries Map of all combatants by CombatantID
 */
case class Roster[T](factory: RosterCombatantFactory[T], entries: Map[CombatantID, T]) {

  /**
   * Adds a combatant to the Roster. If no CombatantID is provided one will be generated by the Roster. If the combatant
   * being added was already present, the factory.replaceDefinition is called to provide an updated copy of the combatant.
   * @param cid Option for a CombatantID, if None the Roster will generate a numeric CombatantID
   * @param alias Alias for the combatant
   * @param entity CombatantEntity with definition of the combatant
   * @return Modified copy of the roster
   */
  def addCombatant(cid: Option[CombatantID], alias: String, entity: CombatantEntity): Roster[T] = {

    def nextId(): String = {
      val numIdSet: Set[Int] = entries.keys.flatMap(x => x.asNumber).toSet
      (1 to 100000000).dropWhile(numIdSet contains).head.toString
    }

    val definition: CombatantRosterDefinition = if (cid.isDefined) {
      CombatantRosterDefinition(cid.get, alias, entity)
    } else {
      CombatantRosterDefinition(CombatantID(nextId()), alias, entity)
    }
    val entry: T = if (entries.isDefinedAt(definition.cid)) {
      factory.replaceDefinition(entries(definition.cid), definition)
    } else {
      factory.createCombatant(definition)
    }
    this.copy(entries = entries.updated(definition.cid, entry))
  }

  /**
   * Provides an unsorted list of all CombatantID defined in the roster
   */
  def allCombatantIDs: List[CombatantID] = entries.keys.toList

  /**
   * Remove elements that match the specified predicate from the Roster
   * @param toClear Test predicate which indicates which entries should be removed, if the value is true.
   * @return Roster with all elements where toClear(x) == true removed.
   */
  def clear(toClear: T => Boolean): Roster[T] = this.copy(entries = entries.filter(p => !toClear(p._2)))

  /**
   * Return combatant defined by combId
   * @param combId ID to look for
   * @return The combatant if found, otherwise will throw a NoSuchElementException
   */
  def combatant(combId: CombatantID): T = entries(combId)

  /**
   * Use to check for the presence of a combatant
   * @param combId CombatantID to search for
   * @return true if combId is present in the roster
   */
  def isDefinedAt(combId: CombatantID): Boolean = entries.isDefinedAt(combId)


  /**
   * Compares all combatant that are in both rosters and returns differences between them.
   */
  def combatantDiff(that: Roster[T]): Set[CombatStateDiff[_]] = {
    //TODO Remove type casting
    val diffs = entries.map(p => (if (that.entries.isDefinedAt(p._1)) p._2.asInstanceOf[Combatant].diff(that.combatant(p._1).asInstanceOf[Combatant]) else Set()))
    diffs.foldLeft(Set.empty[CombatStateDiff[_]])(_ ++ _)
  }
}