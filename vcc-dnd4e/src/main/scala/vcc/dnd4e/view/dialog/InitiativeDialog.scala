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

import scala.swing._
import scala.swing.event._
import vcc.util.swing._

import scala.util.Sorting
import vcc.dnd4e.view.helper.{InitiativeRollEditor, InitiativeRoll}
import vcc.util.{ListHelper}
import vcc.dnd4e.tracker.common.InitiativeDefinition
import vcc.dnd4e.view.{UnifiedSequenceTable, IconLibrary, UnifiedCombatant, PanelDirector}
import vcc.dnd4e.util.DiceBag

class InitiativeDialog(window: Frame, director: PanelDirector, combatState: UnifiedSequenceTable)
  extends ModalPromptDialog[(Boolean, List[InitiativeDefinition])](window, "Roll Initiative") {

  private val table = new RowProjectionTable[InitiativeDialogEntry] with CustomRenderedRowProjectionTable[InitiativeDialogEntry] {
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

  private val rollButton = new Button(Action("Roll") {
    setDialogResultAndClose(Some((false, getResult)))
  })
  private val rollAndStartButton = new Button(Action("Roll and Start Combat") {
    val ir = getResult
    setDialogResultAndClose(Some((!ir.isEmpty, ir))) //Only start if there are initiative
  })

  private val groupCheckbox = new Button(Action("Group similar") {
    table.content = mergeSimilar(table.content.toList)
  })

  private val splitGroup = new Button(Action("Split Group") {
    val sel = getSelectedRow
    if (sel.isDefined) {
      val idx = sel.get
      val toSplit = table.content(idx)
      val segment = List(
        new InitiativeDialogEntry(Set(toSplit.ids.toSeq.take(toSplit.ids.size / 2).toSeq: _*), toSplit.name, toSplit.init, toSplit.roll, toSplit.skip),
        new InitiativeDialogEntry(Set(toSplit.ids.toSeq.drop(toSplit.ids.size / 2).toSeq: _*), toSplit.name, toSplit.init, toSplit.roll, toSplit.skip))
      table.content = ListHelper.splice(table.content.toList, idx, segment)
    }
  })

  private val breakGroup = new Button(Action("Break Group") {
    val sel = getSelectedRow
    if (sel.isDefined) {
      val idx = sel.get
      val toSplit = table.content(idx)
      val segment = toSplit.ids.map(id => new InitiativeDialogEntry(Set(id), toSplit.name, toSplit.init, toSplit.roll, toSplit.skip)).toList
      table.content = ListHelper.splice(table.content.toList, idx, segment)
    }
  })

  contents = new MigPanel("") {
    add(new ScrollPane {
      contents = table
    }, "growx,growy,wrap")
    add(groupCheckbox, "split 4")
    add(splitGroup)
    add(breakGroup, "wrap")
    add(new Label("<html><body style='font-weight: normal; font-size: small;'><b>Roll column can be one of:</b> <dl><dt>blank</dt><dd>will not be in initiative order</dd><dt>r</dt><dd>Roll for me</dd><dt>number</dt><dd>The value of the dice roll</dd></dl><p>Combatant with multiple initiative entries should have <br>several numbers or 'r' separated by slash ('/').</p></body></html>"), "wrap")

    add(rollAndStartButton, "split 4")
    add(rollButton)
    add(new Button(cancelAction), "")
  }
  minimumSize = new java.awt.Dimension(360, 550)
  splitGroup.enabled = false
  breakGroup.enabled = false
  this.placeOnScreenCenter()
  this.peer.setIconImage(IconLibrary.MetalD20.getImage)

  listenTo(table.selection)

  reactions += {
    case TableRowsSelected(t, rng, false) =>
      splitGroup.enabled = {
        val sel = getSelectedRow
        (sel.isDefined) && table.content(sel.get).ids.size > 1
      }
      breakGroup.enabled = splitGroup.enabled
  }

  //Setup table and checkbox
  table.content = {
    val orderedAndFilter: Seq[UnifiedCombatant] = Sorting.stableSort[UnifiedCombatant](
      combatState.elements.filter(e => director.rules.canCombatantRollInitiative(combatState.state, e.combId)).toSeq,
      (a: UnifiedCombatant, b: UnifiedCombatant) => {
        a.combId.id < b.combId.id
      })
    orderedAndFilter.map(
      cmb => new InitiativeDialogEntry(
        Set(cmb.combId), cmb.name, cmb.definition.entity.initiative,
        if (cmb.isInOrder) InitiativeRoll(List(Some(cmb.initiative.initScore))) else InitiativeRoll.simpleRoll,
        cmb.isInOrder)
    ).toList.foldLeft(List[InitiativeDialogEntry]())(joinInitiativeRolls)
  }
  rollAndStartButton.enabled = !combatState.state.isCombatStarted


  def collectResult(): Option[(Boolean, List[InitiativeDefinition])] = {
    None // This should not be called
  }

  private def getResult: List[InitiativeDefinition] = {
    table.content.filter(e => !e.skip).
      map(ie => (ie.ids, ie.init, ie.roll.resolve(ie.init, DiceBag))). //Tuple( Ids, InitBonus, Rolls)
      flatMap(tpl => tpl._1.map(id => InitiativeDefinition(id, tpl._2, tpl._3))).toList
  }

  private def getSelectedRow: Option[Int] = {
    val sel = table.selection.rows.toSeq
    if (sel.isEmpty) None
    else Some(sel(0))
  }

  def insertSimilar(lst: List[InitiativeDialogEntry], e: InitiativeDialogEntry): List[InitiativeDialogEntry] = {
    lst match {
      case head :: rest if (head.isSimilar(e)) => head.merge(e) :: rest
      case head :: rest => head :: insertSimilar(rest, e)
      case Nil => List(e)
    }
  }

  private def joinInitiativeRolls(lst: List[InitiativeDialogEntry], e: InitiativeDialogEntry): List[InitiativeDialogEntry] = {
    lst match {
      case head :: rest if (head.ids == e.ids) =>
        head.roll = head.roll.addRolls(e.roll)
        head :: rest
      case head :: rest => head :: joinInitiativeRolls(rest, e)
      case Nil => List(e)
    }
  }

  def mergeSimilar(lst: List[InitiativeDialogEntry]): List[InitiativeDialogEntry] = {
    lst.foldLeft(List[InitiativeDialogEntry]())(insertSimilar)
  }
}
