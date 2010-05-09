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

import scala.swing._
import scala.swing.event._
import vcc.util.swing._

import scala.util.Sorting
import vcc.dnd4e.view.PanelDirector
import vcc.dnd4e.domain.tracker.common.{CombatantID, InitiativeDefinition}
import java.awt.{Component, Window}
import javax.swing.{JFormattedTextField, JTable, DefaultCellEditor}
import vcc.dnd4e.view.helper.{InitiativeRollEditor, InitiativeRoll}
import vcc.util.DiceBag

class InitiativeDialog(window: Frame, director: PanelDirector) extends ModalDialog[List[InitiativeDefinition]](window, "Roll Initiative") {
  val initTable = new ProjectionTableModel[InitiativeDialogEntry](InitiativeDialogEntryProjection)
  val table = new EnhancedTable {
    model = initTable
    autoResizeMode = Table.AutoResizeMode.Off
    selection.intervalMode = Table.IntervalMode.Single
    setColumnWidth(0, 50)
    setColumnWidth(1, 150)
    setColumnWidth(2, 50)
    setColumnWidth(3, 50)
    scrollablePeer.setDefaultEditor(classOf[InitiativeRoll], new InitiativeRollEditor())
    this.peer.putClientProperty("terminateEditOnFocusLost", true) // Make sure we close the edit
  }

  private val groupCheckbox = new CheckBox("Group similar (same name and initiative bonus)")
  contents = new MigPanel("") {
    add(new ScrollPane {contents = table}, "growx,growy,wrap")
    add(groupCheckbox, "wrap")
    add(new Label("<html><body style='font-weight: normal;'><b>Roll column can be one of:</b> <dl><dt>blank</dt><dd>will not be in initiative order</dd><dt>r</dt><dd>Roll for me</dd><dt>number</dt><dd>The value of the dice roll</dd></dl><p>Combatant with multiple initiative entries should have <br>several numbers or 'r' separated by slash ('/').</p></body></html>"), "wrap")
    add(new Button(okAction), "split 3")
    add(new Button(cancelAction), "")
  }
  minimumSize = new java.awt.Dimension(360, 550)
  this.placeOnScreenCenter()

  println("InitiOrder: " + director.currentState.state.order)
  println("InitiOrder: " + director.currentState.state.combatantsNotInOrder)
  println("Filtered: " + director.currentState.elements.filter(e => director.rules.canCombatantRollInitiative(director.currentState.state, e.combId)))

  initTable.content = Sorting.stableSort[InitiativeDialogEntry](
    director.currentState.elements.filter(e => director.rules.canCombatantRollInitiative(director.currentState.state, e.combId)).map(cmb => new InitiativeDialogEntry(cmb.combId, cmb.name, cmb.definition.entity.initiative, InitiativeRoll.simpleRoll)).toList,
    (a: InitiativeDialogEntry, b: InitiativeDialogEntry) => {a.id.id < b.id.id}).toSeq

  def processOK() {
    //FIXME Initiative rolling has to be remade
    //    dialogResult = Some(InitiativeRoller.rollInitiative(groupCheckbox.selected, initTable.content.toList))
    initTable.content.foreach(x => println(x.id + " roll " + x.roll.rolls))
    dialogResult = Some(initTable.content.map(ie => InitiativeDefinition(ie.id, ie.init, ie.roll.resolve(ie.init, DiceBag))).toList)
    println("Dialog Result: " + dialogResult)
  }
}
