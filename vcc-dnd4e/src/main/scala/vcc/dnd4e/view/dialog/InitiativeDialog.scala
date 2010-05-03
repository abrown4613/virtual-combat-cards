/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.view.dialog

import java.awt.Window
import scala.swing._
import scala.swing.event._
import vcc.util.swing._

import scala.util.Sorting
import vcc.dnd4e.view.PanelDirector
import vcc.dnd4e.domain.tracker.common.{InitiativeDefinition}

class InitiativeDialog(window: Frame, director: PanelDirector) extends ModalDialog[List[InitiativeDefinition]](window, "Roll Initiative") {
  val initTable = new vcc.util.swing.ProjectionTableModel[InitiativeDialogEntry](InitiativeDialogEntryProjection)
  val table = new EnhancedTable {
    model = initTable
    autoResizeMode = Table.AutoResizeMode.Off
    selection.intervalMode = Table.IntervalMode.Single
    setColumnWidth(0, 35)
    setColumnWidth(1, 150)
    setColumnWidth(2, 35)
    setColumnWidth(3, 35)
    setColumnWidth(4, 70)
  }

  private val groupCheckbox = new CheckBox("Group similar (same name and initiative bonus)")
  contents = new MigPanel("") {
    add(new ScrollPane {contents = table}, "growx,growy,wrap")
    add(groupCheckbox, "wrap")
    add(new Button(okAction), "split 3")
    add(new Button(cancelAction), "")
  }
  minimumSize = new java.awt.Dimension(360, 400)

  initTable.content = Sorting.stableSort[InitiativeDialogEntry](
    director.currentState.elements.map(cmb => new InitiativeDialogEntry(cmb.combId, cmb.name, cmb.definition.entity.initiative, 0, false)).toList,
    (a: InitiativeDialogEntry, b: InitiativeDialogEntry) => {a.id.id < b.id.id}).toSeq

  def processOK() {
    //FIXME Initiative rolling has to be remade
    dialogResult = Some(helper.InitiativeRoller.rollInitiative(groupCheckbox.selected, initTable.content.toList))
  }
}
