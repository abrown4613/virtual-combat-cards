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
import vcc.dnd4e.model.PartyBuilder
import vcc.dnd4e.model.PartyBuilder.{EntryDefinition, EntityResolver}
import vcc.dnd4e.compendium.{CharacterSummary, TrapSummary, MonsterSummary, Compendium}

class PartyEditorPresenter(builder: PartyBuilder) {

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

  private var view: PartyEditorView.UIElement = null

  def addEntry(entityId: EntityID) {
    builder.addEncounterParticipant(PartyEditorPresenter.resolveEntity(entityId))
    updateView()
  }

  def setView(view: PartyEditorView.UIElement) {
    this.view = view
  }

  private def updateView() {
    view.setExperienceMessage(builder.encounterExperience + " XP")
    val entries = for (row <- 0 until builder.numberOfRows) yield {
      PartyTableEntry(None, builder.getQuantity(row), None, builder.getName(row), builder.getExperience(row))
    }
    view.setPartyTableContent(entries.toList)
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