/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
import org.uispec4j.assertion.Assertion
import vcc.dnd4e.view.dialog.PartyEditorView.PartyTableEntry
import vcc.dnd4e.tracker.common.CombatantID
import org.uispec4j.{Trigger, UISpecAdapter, Window, UISpecTestCase}
import org.uispec4j.interception.{FileChooserHandler, WindowHandler, WindowInterceptor}
import javax.swing.{JLabel}
import java.io.File
import org.mockito.Matchers._
import vcc.dnd4e.model.{PartyFile, PartyMember, PartyBuilder}
import vcc.dnd4e.view.PanelDirector

class PartyEditorViewTest extends UISpecTestCase {

  private val definition1 = PartyBuilder.EntryDefinition(EntityID.generateRandom(), "creature", 34)

  private var presenter: PartyEditorPresenter = null
  private var uiElement: PartyEditorView.UIElement = null
  private val mockCompendium = new MockedCompendium()
  private val mockPanelDirector = mock(classOf[PanelDirector])

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
    uiElement.setPartyTableContent(1023, Nil)
    assertTrue(checkLabelContent(getMainWindow, "experience-label", "1023 XP (Level 5)"))
  }

  def testUpdateLevelOnPartySizeChangeTo4() {
    uiElement.setPartyTableContent(1023, Nil)
    getMainWindow.getComboBox("party-size").select("4")
    assertTrue(checkLabelContent(getMainWindow, "experience-label", "1023 XP (Level 6)"))
  }

  def testUpdateLevelOnPartySizeChangeTo6() {
    uiElement.setPartyTableContent(1023, Nil)
    getMainWindow.getComboBox("party-size").select("6")
    assertTrue(checkLabelContent(getMainWindow, "experience-label", "1023 XP (Level 3)"))
  }

  def testSetTableEntry() {
    uiElement.setPartyTableContent(0, List(createSingleEntry(Some(CombatantID("A")), "Nick")))
    assertTrue(getMainWindow.getTable("party-table").rowEquals(0,
      Array("ID", "Alias", "Name", "Qty", "XP"),
      Array("A", "Nick", "creature", "1", "34")))
  }

  def testSelectOnlyOneRow() {
    uiElement.setPartyTableContent(0, List(
      createSingleEntry(Some(CombatantID("A")), "Nick"),
      createSingleEntry(None, null)))
    val partyTable = getMainWindow.getTable("party-table")
    partyTable.selectAllRows()
    assertFalse(partyTable.rowIsSelected(0))
    assertTrue(partyTable.rowIsSelected(1))
  }

  def testRemoveRowActionEnabledOnSelect() {
    uiElement.setPartyTableContent(0, List(createSingleEntry(Some(CombatantID("A")), "Nick")))
    val removeButton = getMainWindow.getButton("Remove")
    assertFalse(removeButton.isEnabled)
    getMainWindow.getTable("party-table").selectRow(0)
    assertTrue("Remove not on when selection", removeButton.isEnabled)
  }

  def testRemoveRowAction() {
    uiElement.setPartyTableContent(0, List(createSingleEntry(Some(CombatantID("A")), "Nick")))
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
    uiElement.setPartyTableContent(0, List(createSingleEntry(Some(CombatantID("A")), "Nick")))
    getMainWindow.getTable("party-table").editCell(0, 3, "2", true)

    assertTrue(verify(presenter).changeQuantity(0, 2))
  }

  def testChangeAlias() {
    uiElement.setPartyTableContent(0, List(createSingleEntry(Some(CombatantID("A")), "Nick")))
    getMainWindow.getTable("party-table").editCell(0, 1, "some alias", true)

    assertTrue(verify(presenter).changeAlias(0, "some alias"))
  }

  def testChangeCombatantID() {
    uiElement.setPartyTableContent(0, List(createSingleEntry(Some(CombatantID("A")), "Nick")))
    when(presenter.isValidCombatantID(0, "a")).thenReturn(true)

    getMainWindow.getTable("party-table").editCell(0, 0, "a", true)

    assertTrue(verify(presenter).changeCombatantId(0, "a"))
    assertTrue(wrapWithCheck(verify(presenter, atLeastOnce()).isValidCombatantID(0, "a")))
  }

  def testChangeCombatantIDToInvalid() {
    uiElement.setPartyTableContent(0, List(createSingleEntry(Some(CombatantID("A")), "Nick")))
    when(presenter.isValidCombatantID(0, "-")).thenReturn(false)

    WindowInterceptor.init(new Trigger {
      def run() {
        getMainWindow.getTable("party-table").editCell(0, 0, "-", true)
      }
    }).process(new WindowHandler("Message") {
      def process(window: Window) = {
        assertTrue(window.titleEquals("Invalid Comabatant ID"))
        assertTrue(checkLabelContent(window, "OptionPane.label", "'-' is not a valid Comabatant ID" +
          ", must be unique and contain letter, numbers and underscore"))
        window.getButton("OK").triggerClick()
      }
    }).run()
    assertTrue(wrapWithCheck(verify(presenter, atLeastOnce()).isValidCombatantID(0, "-")))
  }

  def testUpdateQuantityCellWithWrongField() {
    uiElement.setPartyTableContent(0, List(createSingleEntry(Some(CombatantID("A")), "Nick")))
    getMainWindow.getTable("party-table").editCell(0, 3, "a", true)

    assertTrue(verify(presenter, never()).changeQuantity(0, 2))
  }

  def testClearAllButton() {
    getMainWindow.getButton("Clear all").click()
    assertTrue(verify(presenter).clearAll())
  }

  def testLoadFile() {
    val file = new File("some.peml")
    val memberEid = Compendium.activeRepository.getMonsterSummaries()(0).eid
    val member = PartyMember(null, null, memberEid)
    PartyFile.saveToFile(file, List(member, member))
    handleFileLoadMenu(handleAndVerifyLoadDialog().select(file.getName))
    assertTrue(verify(presenter).loadPartyMembers(List(member, member)))
    file.delete()
  }

  def testFileWithBadContent() {
    val file = new File("some.peml")
    val goodList = buildBadPartyFileAndReturnGoodEntries(file)
    file.deleteOnExit()

    openLoadDialog().
      process(handleAndVerifyLoadDialog().select(file.getName)).
      process(handleNotAllLoadedWarning()).
      run()
    assertTrue(verify(presenter).loadPartyMembers(goodList))
  }

  def testAskForLoadButGiveUp() {
    handleFileLoadMenu(handleAndVerifyLoadDialog().cancelSelection())
    assertTrue(verify(presenter, never()).loadPartyMembers(any[List[PartyMember]]))
  }

  def testAddToBattle() {
    getMainWindow.getMenuBar.getMenu("File").getSubMenu("Add to combat").click()
    assertTrue(verify(presenter).addPartyToBattle(any[PanelDirector]))
  }

  def testSaveToFileAndCancel() {
    handleFileSaveDialog(handleAndVerifySaveDialog().cancelSelection)
    assertTrue(verify(presenter, never()).saveToFile(any[File]))
  }

  def testSaveToFileDoIt() {
    val file = new File("file-to-save.peml")
    assertTrue("File must not exist", wrapWithCheck(!file.exists()))
    handleFileSaveDialog(handleAndVerifySaveDialog().select(file))
    assertTrue(verify(presenter).saveToFile(file))
  }

  implicit private def wrapWithCheck(block: => Unit): Assertion = {
    new Assertion {
      def check() {
        block
      }
    }
  }

  private def createWindowAdapter(presenter: PartyEditorPresenter): UISpecAdapter = {
    val view = new PartyEditorView(presenter, mockPanelDirector)
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

  private def checkLabelContent(window: Window, labelName: String, expected1: String): Assertion = {
    new Assertion() {
      def check() {
        val label = window.findSwingComponent(classOf[JLabel], labelName)
        assert(label != null, "Label not found")
        val expected = expected1
        assert(label.getText == expected, "Expected label text: " + expected + " found: " + label.getText)
      }
    }
  }

  private def handleAndVerifyLoadDialog(): FileChooserHandler = {
    FileChooserHandler.
      init().
      assertIsOpenDialog().
      assertAcceptsFilesOnly()
  }

  private def handleAndVerifySaveDialog(): FileChooserHandler = {
    FileChooserHandler.
      init().
      assertIsSaveDialog().
      assertAcceptsFilesOnly()
  }

  private def handleFileSaveDialog(handler: WindowHandler) {
    WindowInterceptor.
      init(getMainWindow.getMenuBar.getMenu("File").getSubMenu("Save ...").triggerClick()).
      process(handler).
      run()
  }

  private def handleFileLoadMenu(handler: WindowHandler) {
    openLoadDialog().process(handler).run()
  }

  private def openLoadDialog(): WindowInterceptor = {
    WindowInterceptor.
      init(getMainWindow.getMenuBar.getMenu("File").getSubMenu("Load ...").triggerClick())
  }

  private def buildBadPartyFileAndReturnGoodEntries(file: File): List[PartyMember] = {
    val member2Eid = Compendium.activeRepository.getMonsterSummaries()(0).eid
    val member2 = PartyMember(null, null, member2Eid)
    val member1 = PartyMember(null, null, EntityID.generateRandom())
    PartyFile.saveToFile(file, List(member1, member2))
    val goodList = List(member2)
    goodList
  }

  private def handleNotAllLoadedWarning(): WindowHandler = {
    new WindowHandler("Warning dialog") {
      def process(window: Window): Trigger = {
        assertTrue(window.titleEquals("Not all entries loaded"))
        window.getButton("OK").triggerClick()
      }
    }
  }
}