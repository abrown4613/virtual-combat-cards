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
package vcc.dnd4e.view.dialog

import vcc.infra.datastore.naming.EntityID
import vcc.dnd4e.view.dialog.PartyEditorView.PartyTableEntry
import vcc.dnd4e.model.PartyBuilder.{EntryDefinition, EntityResolver}
import vcc.dnd4e.compendium.{CharacterSummary, TrapSummary, MonsterSummary, Compendium}
import vcc.dnd4e.tracker.common.CombatantID
import vcc.dnd4e.view.PanelDirector
import java.io.File
import vcc.dnd4e.view.helper.PartyLoader
import vcc.dnd4e.model.{PartyFile, PartyMember, PartyBuilder}

class PartyEditorPresenter(builder: PartyBuilder) {
  private var view: PartyEditorView.UIElement = null

  def this() = this(new PartyBuilder())

  def collapseEntries() {
    builder.collapseSimilar()
    updateView()
  }

  def expandEntries() {
    builder.expandSimilar()
    updateView()
  }

  def changeQuantity(row: Int, newQuantity: Int) {
    builder.setQuantity(row, newQuantity)
    updateView()
  }

  def changeAlias(row: Int, aliasText: String) {
    builder.setAlias(row, Some(aliasText))
    updateView()
  }

  def changeCombatantId(row: Int, cid: String) {
    val cidOption = if (cid == null || cid == "") None else Some(CombatantID(cid))
    builder.setId(row, cidOption)
    updateView()
  }

  def addEntry(entityId: EntityID) {
    builder.addEncounterParticipant(PartyEditorPresenter.resolveEntity(entityId))
    updateView()
  }

  def setView(view: PartyEditorView.UIElement) {
    this.view = view
  }

  def isValidCombatantID(rowToSet: Int, combatantId: String): Boolean = {
    if (combatantId == "")
      true
    else
      CombatantID.isValidID(combatantId) && !builder.wouldViolateIdUniqueness(CombatantID(combatantId), rowToSet)
  }

  private def updateView() {
    val entries = for (row <- 0 until builder.numberOfRows) yield {
      PartyTableEntry(
        builder.getId(row), builder.getQuantity(row),
        builder.getAlias(row), builder.getName(row), builder.getExperience(row))
    }
    view.setPartyTableContent(builder.encounterExperience, entries.toList)
  }

  def clearAll() {
    builder.clear()
    updateView()
  }

  private def partyMembersWithIDFirst(list:Seq[PartyMember]): Seq[PartyMember] = {
    val (withId, withoutId) = list.partition(pm => pm.id != null)
    val finalList = withId ++ withoutId
    finalList
  }

  def loadPartyMembers(list: Seq[PartyMember]) {
    val finalList: Seq[PartyMember] = partyMembersWithIDFirst(list)
    for (element <- finalList) {
      val idx = builder.numberOfRows
      builder.addEncounterParticipant(PartyEditorPresenter.resolveEntity(element.eid))
      builder.setAlias(idx, Option(element.alias))
      builder.setId(idx, Option(element.id))
    }
    updateView()
  }

  def addPartyToBattle(director: PanelDirector) {
    val toAdd = partyMembersWithIDFirst(builder.generateParty())
    PartyLoader.getInstance(director, null).loadToBattle(toAdd)
  }

  def saveToFile(file: File) {
    PartyFile.saveToFile(file, partyMembersWithIDFirst(builder.generateParty()))
  }
}

object PartyEditorPresenter extends EntityResolver {
  def resolveEntity(entityId: EntityID): EntryDefinition = {
    val summary = Compendium.activeRepository.getEntitySummary(entityId)
    summary match {
      case ms: MonsterSummary => EntryDefinition(ms.eid, ms.name, ms.xp)
      case ts: TrapSummary => EntryDefinition(ts.eid, ts.name, ts.xp)
      case cs: CharacterSummary => EntryDefinition(cs.eid, cs.name, 0)
    }
  }
}