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
package vcc.dnd4e.view.compendium

import org.uispec4j.finder.ComponentMatcher
import java.awt.Component
import org.uispec4j.assertion.Assertion
import org.uispec4j.assertion.testlibrairies.AssertAdapter
import org.xhtmlrenderer.simple.XHTMLPanel
import javax.swing.JLabel
import org.uispec4j._
import interception.{FileChooserHandler, WindowInterceptor}
import org.mockito.Mockito._
import org.mockito.Matchers
import vcc.infra.datastore.naming.EntityID
import vcc.util.swing.{XHTMLPaneAgent, XHTMLPane}
import java.io.File

class ImportToolViewTest extends UISpecTestCase {

  private var view: ImportToolView.UserView = null
  private val presenter = mock(classOf[ImportToolView.Presenter])

  override def setUp() {
    super.setUp()
    XHTMLPaneAgent.createInstance(new java.io.File("./fs-wc"))

    val frame = new ImportToolView(presenter)
    view = frame
    setAdapter(new UISpecAdapter() {
      def getMainWindow: Window = {
        new Window(frame.peer)
      }
    })
  }

  def testBasicLayout() {
    assertTrue(getMainWindow.titleEquals("Import Tool"))
    assertTrue(getImportButton.isEnabled)
    assertTrue(getList.isEnabled)
    assertTrue(getImportingBar.isEnabled)
    assertTrue("Can't find XHTMLPanel", getXHTMLPanel != null)
    assertTrue("Can't find progress label", getImportingLabel.isEnabled)
    verify(presenter).registerView(view)
  }

  def testUpdateProgress() {
    view.updateProgress(1, 3)
    assertTrue(getImportingBar.completionEquals(33))
    assertTrue(getImportingLabel.getText == "Imported 1 of 3")
  }

  def testUpdateProgressComplete() {
    view.updateProgress(0, 0)
    assertFalse(getImportingBar.isVisible)
    assertFalse(getImportingLabel.isVisible)
  }

  def testImportSomeFiles() {
    val files = Seq("file.1", "file.2").map(new File(_))
    WindowInterceptor.
      init(getImportButton.triggerClick()).
      process(fileDialogHandler.select(files.toArray)).
      run()
    verify(presenter).processFiles(files)
  }

  def testClickImportAndCancel() {
    WindowInterceptor.
      init(getImportButton.triggerClick()).
      process(fileDialogHandler.cancelSelection()).
      run()
    verify(presenter, never()).processFiles(Matchers.any(classOf[Seq[File]]))
  }

  def testSettingContent() {
    view.setListContent(List((EntityID.generateRandom(), "Item 1")))
    assertTrue(getList.contentEquals("Item 1"))
  }

  def testSelectAnImportedEntry() {
    val eid = EntityID.generateRandom()
    view.setListContent(List((EntityID.generateRandom(), "Item 0"), (eid, "Item 1")))
    getList.selectIndex(1)
    verify(presenter).selectStatBlock(eid)
  }

  def testAllowOnlyOneSelection() {
    view.setListContent(List((EntityID.generateRandom(), "Item 0"), (EntityID.generateRandom(), "Item 1")))
    getList.selectIndices(0, 1)
    assertTrue(getList.selectionEquals("Item 1"))
  }

  def testCantSelectHitEntry() {
    getList.selectIndex(0)
    verify(presenter, never()).selectStatBlock(Matchers.any(classOf[EntityID]))
  }

  def testUpdateStatBlock() {
    val doc = XHTMLPane.parsePanelDocument("<html><body><h1>Title</h1></body></html>")
    println(XHTMLPaneAgent.getInstance())
    println(doc.getDocumentElement.getTextContent)
    view.setStatBlock(doc)
    assertTrue(getXHTMLPanel.getDocument == doc)
  }

  private def fileDialogHandler: FileChooserHandler = {
    FileChooserHandler.
      init().assertAcceptsFilesOnly().
      assertIsOpenDialog().
      titleEquals("Select files to import").
      assertMultiSelectionEnabled(true)
  }

  private def getImportingBar: ProgressBar = {
    getMainWindow.getProgressBar("importing-progress")
  }

  private def getImportingLabel: JLabel = {
    getMainWindow.findSwingComponent(classOf[JLabel], "importing-label")
  }

  private def getList: ListBox = {
    getMainWindow.getListBox("imported-list")
  }

  private def getImportButton: Button = {
    getMainWindow.getButton("Import File")
  }

  implicit private def makeAssertion(test: => Boolean): Assertion = new Assertion {
    def check() {
      AssertAdapter.assertTrue(test)
    }
  }

  private def getXHTMLPanel: XHTMLPanel = {
    getMainWindow.findSwingComponent(new ComponentMatcher {
      def matches(component: Component): Boolean = {
        component match {
          case _: XHTMLPanel => true
          case _ => false
        }
      }
    }).asInstanceOf[XHTMLPanel]
  }
}