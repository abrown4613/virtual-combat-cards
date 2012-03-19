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

import vcc.dnd4e.view.IconLibrary
import vcc.util.swing.MigPanel
import swing._
import event.ButtonClicked
import vcc.dnd4e.compendium.view.CompendiumEntitySelectionPanel
import vcc.dnd4e.tracker.common.CombatantID
import javax.swing.table.AbstractTableModel
import vcc.dnd4e.view.dialog.PartyEditorView.{PartyTableEntry}
import javax.swing.{ListSelectionModel, JTable}
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import java.lang.{Class, String}

object PartyEditorView {

  case class PartyTableEntry(id: Option[CombatantID], quantity: Int,
                             alias: Option[String], name: String, experience: Int)

  trait UIElement {
    def setExperienceMessage(newMessage: String)

    def setPartyTableContent(content: List[PartyTableEntry])
  }

}

class PartyEditorView(presenter: PartyEditorPresenter) extends Frame with PartyEditorView.UIElement {

  private class PartyTableModel extends AbstractTableModel {
    private var content: List[PartyTableEntry] = Nil
    private val columns = Array("ID", "Alias", "Name", "Qty", "XP")

    def getRowCount: Int = content.length

    def getColumnCount: Int = 5

    def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = {
      val row = content(rowIndex)
      columnIndex match {
        case 0 => row.id.map(_.id).getOrElse("")
        case 1 => row.alias.getOrElse("")
        case 2 => row.name
        case 3 => row.quantity.asInstanceOf[java.lang.Integer]
        case 4 => row.experience.toString
      }
    }

    override def getColumnName(column: Int): String = columns(column)

    def setContent(newContent: List[PartyTableEntry]) {
      content = newContent
      fireTableDataChanged()
    }

    override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = {
      Set(3, 1, 0) contains columnIndex
    }

    override def setValueAt(aValue: AnyRef, rowIndex: Int, columnIndex: Int) {
      columnIndex match {
        case 0 => validateAndUpdateCombatantID(rowIndex, aValue.asInstanceOf[String])
        case 1 => presenter.changeAlias(rowIndex, aValue.asInstanceOf[String])
        case 3 => presenter.changeQuantity(rowIndex, aValue.asInstanceOf[java.lang.Integer].toInt)
      }
    }


    override def getColumnClass(columnIndex: Int): Class[_] = {
      if (columnIndex == 3)
        classOf[java.lang.Integer]
      else
        classOf[String]
    }
  }

  def this() = this(new PartyEditorPresenter())

  private val compendiumEntries = new CompendiumEntitySelectionPanel
  private val totalXPLabel = new Label()
  totalXPLabel.name = "experience-label"
  private val partySizeCombo = new ComboBox[Int]((1 to 10).toSeq)
  partySizeCombo.selection.item = 5
  private val collapseCheckBox = new CheckBox("Collapse similar entries")
  private val addButton = new Button(Action("Add to Party >>") {
    val sel = compendiumEntries.currentSelection
    if (sel.isDefined) {
      presenter.addEntry(sel.get.eid)
    }
  })
  addButton.tooltip = "Double clicking on entries from the right table will also add to party."

  private val removeButton = new Button(Action(" << Remove") {
    presenter.changeQuantity(table.getSelectedRow, 0)
  })
  removeButton.enabled = false

  private val clearAllButton = new Button(Action("Clear all") {
    presenter.clearAll()
  })

  private val tableModel = new PartyTableModel()
  private val table = new JTable(tableModel)
  table.setName("party-table")
  table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
  table.getSelectionModel.addListSelectionListener(new ListSelectionListener {
    def valueChanged(e: ListSelectionEvent) {
      removeButton.enabled = (table.getSelectedRows.length > 0)
    }
  })

  compendiumEntries.doubleClickAction = addButton.action
  title = "Edit Encounter and Party"
  iconImage = IconLibrary.MetalD20.getImage
  contents = createContentPanel()
  menuBar = createMenuBar()

  presenter.setView(this)

  def setExperienceMessage(newMessage: String) {
    totalXPLabel.text = newMessage
  }

  def setPartyTableContent(content: List[PartyTableEntry]) {
    tableModel.setContent(content)
  }

  private def createContentPanel(): MigPanel = {
    new MigPanel("fill,flowy", "[350][fill,growprio 0][350,fill]", "[]") {
      add(compendiumEntries, "growy, growx,wrap")
      add(addButton, "split 3")
      add(removeButton, "growx")
      add(clearAllButton, "growx,wrap")
      add(new MigPanel("fill,ins 0") {
        add(new Label("Party Size:"), "split 3")
        add(partySizeCombo, "")
        add(totalXPLabel, "gap 20px, align right, wrap")
        add(collapseCheckBox, "wrap")
        add(new ScrollPane(Component.wrap(table)), "span 2,grow")
      }, "grow")
    }
  }

  listenTo(this.collapseCheckBox)
  reactions += {
    case ButtonClicked(this.collapseCheckBox) =>
      if (collapseCheckBox.selected)
        presenter.collapseEntries()
      else
        presenter.expandEntries()
  }


  private def createMenuBar() = {
    val mb = new MenuBar()
    val fileMenu = new Menu("File")
    mb.contents += fileMenu
    fileMenu.contents += new MenuItem(Action("Save ...") {
      //doSave()
    })
    fileMenu.contents += new MenuItem(Action("Load ...") {
      //doLoad()
    })
    fileMenu.contents += new Separator()
    fileMenu.contents += new MenuItem(Action("Add to combat") {
      //doAddToCombat()
    })
    mb
  }

  private def validateAndUpdateCombatantID(rowIndex: Int, newId: String) {
    if (!presenter.isValidCombatantID(rowIndex, newId))
      scala.swing.Dialog.showMessage(removeButton,
        "'" + newId + "' is not a valid Comabatant ID, must be unique and contain letter, numbers and underscore",
        "Invalid Comabatant ID", Dialog.Message.Error)
    else
      presenter.changeCombatantId(rowIndex, newId)
  }

}