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
import vcc.dnd4e.domain.tracker.common.{CombatantID, InitiativeDefinition}
import vcc.dnd4e.view.helper.{InitiativeRollEditor, InitiativeRoll}
import vcc.dnd4e.view.{UnifiedCombatant, PanelDirector}
import vcc.util.{ListHelper, DiceBag}

class InitiativeDialog(window: Frame, director: PanelDirector) extends ModalDialog[List[InitiativeDefinition]](window, "Roll Initiative") {
  val table = new RowProjectionTable[InitiativeDialogEntry] with CustomRenderedRowProjectionTable[InitiativeDialogEntry] {
    projection = new ProjectionTableModel[InitiativeDialogEntry](InitiativeDialogEntryProjection)
    val labelFormatter = new InitiativeDialogEntryFormatter()
    autoResizeMode = Table.AutoResizeMode.Off
    selection.intervalMode = Table.IntervalMode.Single
    setColumnWidth(0, 50)
    setColumnWidth(1, 150)
    setColumnWidth(2, 50)
    setColumnWidth(3, 50)
    scrollablePeer.setDefaultEditor(classOf[InitiativeRoll], new InitiativeRollEditor())
    this.peer.putClientProperty("terminateEditOnFocusLost", true) // Make sure we close the edit
  }

  private val groupCheckbox = new Button(Action("Group similar") {
    table.content = mergeSimilar(table.content.toList)
  })
  private val splitGroup = new Button(Action("Split Group") {
    val sel = table.selection.rows.toSeq
    if (!sel.isEmpty) {
      val idx = sel(0)
      val toSplit = table.content(idx)
      val segment = List(
        new InitiativeDialogEntry(Set(toSplit.ids.toSeq.take(toSplit.ids.size / 2).toSeq: _*), toSplit.name, toSplit.init, toSplit.roll),
        new InitiativeDialogEntry(Set(toSplit.ids.toSeq.drop(toSplit.ids.size / 2).toSeq: _*), toSplit.name, toSplit.init, toSplit.roll))
      table.content = ListHelper.splice(table.content.toList, idx, segment)
    }
  })

  private val breakGroup = new Button(Action("Break Group") {
    val sel = table.selection.rows.toSeq
    if (!sel.isEmpty) {
      val idx = sel(0)
      val toSplit = table.content(idx)
      val segment = toSplit.ids.map(id => new InitiativeDialogEntry(Set(id), toSplit.name, toSplit.init, toSplit.roll)).toList
      table.content = ListHelper.splice(table.content.toList, idx, segment)
    }
  })

  contents = new MigPanel("") {
    add(new ScrollPane {contents = table}, "growx,growy,wrap")
    add(groupCheckbox, "split 4")
    add(splitGroup)
    add(breakGroup, "wrap")
    add(new Label("<html><body style='font-weight: normal;'><b>Roll column can be one of:</b> <dl><dt>blank</dt><dd>will not be in initiative order</dd><dt>r</dt><dd>Roll for me</dd><dt>number</dt><dd>The value of the dice roll</dd></dl><p>Combatant with multiple initiative entries should have <br>several numbers or 'r' separated by slash ('/').</p></body></html>"), "wrap")
    add(new Button(okAction), "split 3")
    add(new Button(cancelAction), "")
  }
  minimumSize = new java.awt.Dimension(360, 550)
  this.placeOnScreenCenter()

  private val orderedAndFilter: Seq[UnifiedCombatant] = Sorting.stableSort[UnifiedCombatant](director.currentState.elements.filter(e => director.rules.canCombatantRollInitiative(director.currentState.state, e.combId)), (a: UnifiedCombatant, b: UnifiedCombatant) => {a.combId.id < b.combId.id})
  table.content = orderedAndFilter.map(cmb => new InitiativeDialogEntry(Set(cmb.combId), cmb.name, cmb.definition.entity.initiative, InitiativeRoll.simpleRoll)).toList

  def processOK() {
    dialogResult = Some(table.content.flatMap(ie => ie.ids.map(id => InitiativeDefinition(id, ie.init, ie.roll.resolve(ie.init, DiceBag)))).toList)
  }

  def insertSimilar(lst: List[InitiativeDialogEntry], e: InitiativeDialogEntry): List[InitiativeDialogEntry] = {
    lst match {
      case head :: rest if (head.isSimilar(e)) => head.merge(e) :: rest
      case head :: rest => head :: insertSimilar(rest, e)
      case Nil => List(e)
    }
  }

  def mergeSimilar(lst: List[InitiativeDialogEntry]): List[InitiativeDialogEntry] = {
    lst.foldLeft(List[InitiativeDialogEntry]())(insertSimilar)
  }

}
