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

import org.mockito.Mockito._
import vcc.infra.datastore.naming.EntityID
import vcc.dnd4e.compendium._
import org.uispec4j.{UISpecAdapter, Window, UISpecTestCase}
import javax.swing.{JLabel, JFrame}
import org.uispec4j.assertion.Assertion
import vcc.dnd4e.view.dialog.PartyEditorView.PartyTableEntry
import vcc.dnd4e.model.PartyBuilder
import vcc.dnd4e.tracker.common.CombatantID

object MockedPartyEditor {
  def main(args: Array[String]) {

    new MockedCompendium().initialize()

    val view = new PartyEditorView()
    view.peer.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    view.visible = true
  }
}

class MockedCompendium {

  private val monsterEntries = createMonsterEntries(10)
  private val trapEntries = createTrapEntries(10)
  private val characterEntries = createCharacterEntries(10)

  def initialize() {
    val mockRepository = createMockCompendium()
    Compendium.setActiveRepository(mockRepository)
  }


  private def createMockCompendium(): CompendiumRepository = {
    val mockRepository: CompendiumRepository = mock(classOf[CompendiumRepository])
    when(mockRepository.getMonsterSummaries()).thenReturn(monsterEntries)
    when(mockRepository.getCharacterSummaries()).thenReturn(characterEntries)
    when(mockRepository.getTrapSummaries()).thenReturn(trapEntries)

    mockFetch(mockRepository, monsterEntries)
    mockFetch(mockRepository, trapEntries)
    mockFetch(mockRepository, characterEntries)

    mockRepository
  }

  private def mockFetch(mockRepository: CompendiumRepository, entries: Seq[EntitySummary]) {
    entries.foreach(entry => when(mockRepository.getEntitySummary(entry.eid)).thenReturn(entry))
  }

  private def createTrapEntries(maxLevel: Int): Seq[TrapSummary] = {
    (1 to maxLevel).map {
      level =>
        TrapSummary(EntityID.generateRandom(), Compendium.monsterClassID,
          "Trap " + level, level, level * 100, "Trap", false)
    }
  }

  private def createCharacterEntries(maxLevel: Int): Seq[CharacterSummary] = {
    (1 to maxLevel).map {
      level =>
        CharacterSummary(EntityID.generateRandom(), Compendium.monsterClassID,
          "Character " + level, level, "Human", "Fighter")
    }
  }

  private def createMonsterEntries(maxLevel: Int): Seq[MonsterSummary] = {
    (1 to maxLevel).map {
      level =>
        MonsterSummary(EntityID.generateRandom(), Compendium.monsterClassID,
          "Monster " + level, level, level * 100, "soldier", false)
    }
  }
}

class PartyEditorViewTest extends UISpecTestCase {

  private val definition1 = PartyBuilder.EntryDefinition(EntityID.generateRandom(), "creature", 34)

  private var presenter: PartyEditorPresenter = null
  private var uiElement: PartyEditorView.UIElement = null
  private val mockCompendium = new MockedCompendium()

  override def setUp() {
    super.setUp()
    presenter = mock(classOf[PartyEditorPresenter])
    mockCompendium.initialize()
    setAdapter(createWindowAdapter(presenter))
  }

  def testShowAndCancel() {
    assertTrue(getMainWindow.titleContains("Edit Encounter and Party"))
    assertTrue(getMainWindow.getTable("party-table").rowCountEquals(0))
    assertTrue(getMainWindow.getTable("creature-view").rowCountEquals(10))
    verify(presenter).setView(uiElement)
  }

  def testSelectCreatureAndAdd() {
    getMainWindow.getTable("creature-view").selectRow(0)
    assertTrue(getMainWindow.getTable("creature-view").rowsAreSelected(0))
    getMainWindow.getButton("Add").click()

    assertTrue(verify(presenter).addEntry(Compendium.activeRepository.getMonsterSummaries()(0).eid))
  }

  def testMessageOnExperiencePoints() {
    uiElement.setExperienceMessage("Ipsum lorem")
    assertTrue(new Assertion() {
      def check() {
        val label = getMainWindow.findSwingComponent(classOf[JLabel], "experience-label")
        assert(label != null, "Label not found")
        val expected = "Ipsum lorem"
        assert(label.getText == expected, "Expected label text:" + expected + " found: " + label.getText)
      }
    })
  }

  def testSetTableEntry() {
    uiElement.setPartyTableContent(List(createSingleEntry(Some(CombatantID("A")), "Nick")))
    assertTrue(getMainWindow.getTable("party-table").rowEquals(0,
      Array("ID", "Alias", "Name", "Qty", "XP"),
      Array("A", "Nick", "creature", "1", "34")))
  }

  def testSelectOnlyOneRow() {
    uiElement.setPartyTableContent(List(
      createSingleEntry(Some(CombatantID("A")), "Nick"),
      createSingleEntry(None, null)))
    val partyTable = getMainWindow.getTable("party-table")
    partyTable.selectAllRows()
    assertFalse(partyTable.rowIsSelected(0))
    assertTrue(partyTable.rowIsSelected(1))
  }

  def testRemoveRowActionEnabledOnSelect() {
    uiElement.setPartyTableContent(List(createSingleEntry(Some(CombatantID("A")), "Nick")))
    val removeButton = getMainWindow.getButton("Remove")
    assertFalse(removeButton.isEnabled)
    getMainWindow.getTable("party-table").selectRow(0)
    assertTrue("Remove not on when selection", removeButton.isEnabled)
  }

  def testRemoveRowAction() {
    uiElement.setPartyTableContent(List(createSingleEntry(Some(CombatantID("A")), "Nick")))
    val removeButton = getMainWindow.getButton("Remove")
    getMainWindow.getTable("party-table").selectRow(0)
    removeButton.click()
    assertTrue(verify(presenter).changeQuantity(0, 0))
}

  def testCollapseAndExpandEntries() {
    val checkBox = getMainWindow.getCheckBox("Collapse Similar Entries")
    checkBox.click()
    assertTrue(verify(presenter).collapseEntries())
    checkBox.click()
    assertTrue(verify(presenter).expandEntries())
  }

  def testUpdateQuantityCell() {
    uiElement.setPartyTableContent(List(createSingleEntry(Some(CombatantID("A")), "Nick")))
    getMainWindow.getTable("party-table").editCell(0, 3, "2", true)

    assertTrue(verify(presenter).changeQuantity(0, 2))
  }

  def testUpdateQuantityCellWithWrongField() {
    uiElement.setPartyTableContent(List(createSingleEntry(Some(CombatantID("A")), "Nick")))
    getMainWindow.getTable("party-table").editCell(0, 3, "a", true)

    assertTrue(verify(presenter, never()).changeQuantity(0, 2))
  }

  implicit private def wrapWithCheck(block: => Unit): Assertion = {
    new Assertion {
      def check() {
        block
      }
    }
  }

  private def createWindowAdapter(presenter: PartyEditorPresenter): UISpecAdapter = {
    val view = new PartyEditorView(presenter)
    val window = new Window(view.peer)
    uiElement = view

    new UISpecAdapter {
      def getMainWindow: Window = {
        window
      }
    }
  }

  private def createSingleEntry(id: Option[CombatantID], alias: String): PartyEditorView.PartyTableEntry = {
    PartyTableEntry(id, 1, Option(alias), definition1.name, definition1.experience)
  }

}