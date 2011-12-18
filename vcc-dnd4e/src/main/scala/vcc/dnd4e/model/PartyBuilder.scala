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
package vcc.dnd4e.model

import vcc.infra.datastore.naming.EntityID
import vcc.dnd4e.tracker.common.CombatantID
import collection.immutable.List

object PartyBuilder {

  class CombatantIdMustBeUniqueException(combatantId: CombatantID) extends RuntimeException

  case class EntryDefinition(entityId: EntityID, name: String, experience: Int)

  trait EntityResolver {
    def resolveEntity(entityId: EntityID): EntryDefinition
  }

  private class Entry(val definition: EntryDefinition) {
    def singleElement(): Entry = {
      val entry = new Entry(definition)
      entry.alias = alias
      entry.id = id
      entry
    }

    def expanded: Seq[Entry] = {
      if (quantity > 1)
        (1 to quantity).map(_ => singleElement())
      else
        Seq(this)
    }

    def isSimilarTo(other: Entry): Boolean = {
      this.definition == other.definition &&
        !this.id.isDefined && !other.id.isDefined &&
        this.alias == other.alias
    }

    var quantity: Int = 1
    var alias: Option[String] = None
    var id: Option[CombatantID] = None

    def toPartyMember = {
      PartyMember(
        id.getOrElse(null),
        alias.getOrElse(null),
        definition.entityId)
    }
  }

}

class PartyBuilder {

  import PartyBuilder._

  private var entries: List[Entry] = Nil

  private var groupSimilar = false

  def numberOfRows: Int = entries.size

  def encounterExperience: Int = entries.foldLeft(0)((sum, entry) => sum + entry.quantity * entry.definition.experience)

  def getQuantity(row: Int): Int = entries(row).quantity

  def getName(row: Int): String = entries(row).definition.name

  def getExperience(row: Int): Int = entries(row).definition.experience

  def getId(row: Int): Option[CombatantID] = entries(row).id

  def setId(row: Int, idOption: Option[CombatantID]) {
    checkIdUniqueness()
    splitQuantitySingleAndRemainder(row)

    entries(row).id = idOption
    adjustRows()

    def checkIdUniqueness() {
      if (idOption.isDefined && wouldViolateIdUniqueness(idOption.get, row))
        throw new CombatantIdMustBeUniqueException(idOption.get)
    }

    def splitQuantitySingleAndRemainder(row: Int) {
      if (entries(row).quantity > 1) {
        val (before, after) = entries.splitAt(row)
        entries(row).quantity -= 1
        entries = before ::: List(entries(row).singleElement()) ::: after
      }
    }
  }

  def wouldViolateIdUniqueness(combatantId: CombatantID, rowToSet: Int): Boolean = {
    val toFind = Some(combatantId)
    entries(rowToSet).id != toFind && entries.exists(entry => entry.id == toFind)
  }

  def getAlias(row: Int): Option[String] = entries(row).alias

  def setAlias(row: Int, aliasOption: Option[String]) {
    entries(row).alias = aliasOption
    adjustRows()
  }

  def addEncounterParticipant(definition: EntryDefinition = null) {
    entries = entries ::: List(new Entry(definition))
    adjustRows()
  }

  def collapseSimilar() {
    groupSimilar = true
    compactList()
  }

  def expandSimilar() {
    groupSimilar = false
    expandList()
  }

  def removeRow(row: Int) {
    val (before, after) = entries.splitAt(row)
    entries = before ++ after.tail
  }

  def setQuantity(row: Int, newQuantity: Int) {
    if (newQuantity == 0) {
      removeRow(row)
    } else {
      entries(row).quantity = newQuantity
      adjustRows()
    }
  }

  def clear() {
    entries = Nil
  }

  def loadFromParty(entityResolver: EntityResolver, partyMembers: List[PartyMember]) {
    var usedIds = Set.empty[CombatantID]

    entries = for (member <- partyMembers) yield {
      val entry = new Entry(entityResolver.resolveEntity(member.eid))
      setIdIfNotDuplicate(Option(member.id), entry)
      entry.alias = Option(member.alias)
      entry
    }
    adjustRows()

    def setIdIfNotDuplicate(idOption: Option[CombatantID], entry: PartyBuilder.Entry) {
      if (idOption.isDefined && !usedIds.contains(idOption.get)) {
        usedIds = usedIds + idOption.get
        entry.id = idOption
      }
    }
  }

  def generateParty(): List[PartyMember] = {
    entries.flatMap(_.expanded).map(_.toPartyMember)
  }

  private def adjustRows() {
    if (groupSimilar) compactList()
    else expandSimilar()
  }

  private def compactList() {
    def mergeSimilar(accumulated: List[Entry], entry: Entry): List[Entry] = {
      val o = accumulated.find(p => p.isSimilarTo(entry))
      if (o.isDefined) {
        o.get.quantity += 1
        accumulated
      } else {
        accumulated ::: List(entry)
      }
    }
    entries = entries.foldLeft(List.empty[Entry])(mergeSimilar)
  }

  private def expandList() {
    entries = entries.flatMap(entry => entry.expanded)
  }
}