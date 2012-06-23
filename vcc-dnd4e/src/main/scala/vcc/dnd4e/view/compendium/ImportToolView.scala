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

import vcc.util.swing.{XHTMLPane, MigPanel}
import swing._
import vcc.infra.datastore.naming.EntityID
import javax.swing.JFileChooser
import swing.ListView.IntervalMode
import org.w3c.dom.Document

object ImportToolView {

  trait UserView {
    def setStatBlock(document: Document)

    def updateProgress(done: Int, total: Int)

    def setListContent(content: List[(EntityID, String)])
  }

  trait Presenter {
    def registerView(view: UserView)

    def processFiles(files: Seq[String])

    def selectStatBlock(eid: EntityID)
  }

}

class ImportToolView(presenter: ImportToolView.Presenter) extends Frame with ImportToolView.UserView {

  private var listContent: List[(EntityID, String)] = Nil
  private val importButton = makeImportButton()
  private val importedList = makeImportList()
  private val statBlockPane = new XHTMLPane()
  private val importProgress = makeProgressBar()
  private val importLabel = makeImportLabel()

  initView()

  def updateProgress(done: Int, total: Int) {
    if (total > 0) {
      importProgress.value = (100 * done) / total
      importLabel.text = "Imported " + done + " of " + total
      importProgress.visible = true
      importLabel.visible = true
    } else {
      importProgress.visible = false
      importLabel.visible = false
    }
  }

  def setListContent(content: List[(EntityID, String)]) {
    listContent = content
    importedList.listData = content.map(x => x._2)
  }

  def setStatBlock(document: Document) {
    statBlockPane.setDocument(document)
  }

  private def initView() {
    title = "Import Tool"
    presenter.registerView(this)

    contents = new MigPanel("flowy, insets dialog, hidemode 2", "[300, grow 37]10[500, grow 63]", "[][grow 100,fill][][][]") {
      add(new Label("Imported objects"), "h pref!")
      add(new ScrollPane(importedList), "h 300, gp 100, growx 100, growy 100")
      add(importProgress, "growy 0")
      add(importLabel, "growy 0")
      add(importButton, "growy 0, wrap")
      add(new Label("Imported Stat Block"))
      add(statBlockPane, "growy 100, growx 100, spany 3")
    }

    listenTo(importedList.selection)

    reactions += {
      case swing.event.SelectionChanged(`importedList`) =>
        val selection = importedList.selection.indices.toSeq.headOption
        if (selection.isDefined && selection.get < listContent.length)
          presenter.selectStatBlock(listContent(selection.get)._1)
    }
  }

  private def makeImportButton(): Button = {
    new Button(Action("Import File") {
      val chooser = new JFileChooser()
      chooser.setDialogTitle("Select files to import")
      chooser.setMultiSelectionEnabled(true)
      chooser.showOpenDialog(peer)
      val files = chooser.getSelectedFiles.map(x => x.getName)
      if (!files.isEmpty)
        presenter.processFiles(files)
    })
  }

  private def makeImportList(): ListView[String] = {
    val lv = new ListView[String](List("Drag and Drop files here"))
    lv.name = "imported-list"
    lv.selection.intervalMode = IntervalMode.Single
    lv
  }

  private def makeProgressBar(): ProgressBar = {
    val pb = new ProgressBar()
    pb.name = "importing-progress"
    pb
  }

  private def makeImportLabel() = {
    val label = new Label(" ")
    label.name = "importing-label"
    label
  }

}