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
package vcc.dnd4e.view

import dnd.UnifiedCombatantActionTransfer
import scala.swing._
import scala.swing.event._
import vcc.util.swing._
import vcc.infra.docking._
import tabular._
import vcc.dnd4e.tracker.common.Command.AddEffect
import vcc.dnd4e.tracker.common._
import vcc.util.swing.dnd.{CellDrop, TableCellDropTransferHandler}

class SequenceTable(director: PanelDirector) extends ScrollPane
with ContextObserver with CombatStateObserver with ScalaDockableComponent {

  private val table = new RowProjectionTable[UnifiedCombatant] with CustomRenderedRowProjectionTable[UnifiedCombatant] {
    val labelFormatter = new CombatantStateTableColorer()
    projection = new ProjectionTableModel[UnifiedCombatant](tabular.CombatantStateProjection)
    autoResizeMode = Table.AutoResizeMode.Off
    selection.intervalMode = Table.IntervalMode.Single
    setColumnWidth(0, 25)
    setColumnWidth(1, 150)
    setColumnWidth(2, 70)
    setColumnWidth(3, 70)
    setColumnWidth(4, 50)
    setColumnWidth(5, 70)
    peer.setRowHeight(24)
  }

  val dockID = DockID("sequence")
  val dockTitle = "Initiative Order"
  val dockFocusComponent = table.peer

  private var source: Option[UnifiedCombatantID] = None
  private var target: Option[UnifiedCombatantID] = None
  private var state:UnifiedSequenceTable = null
  private var mustPreventUpdateOfSelection = false

  private val setAction = Action("sequence.setacting") {
    updateActingCombatantLater(target)
  }

  this.contents = table
  initializeKeyBindings()
  initializeDragAndDrop()
  listenTo(table.selection, table)

  reactions += {
    case TableRowsSelected(t, rng, false) if (!mustPreventUpdateOfSelection) =>
      val l = table.selection.rows.toSeq
      if (!l.isEmpty) {
        val c = table.content(l(0))
        updateTargetSelectionLater(Some(c.unifiedId))
      }
    case FocusGained(this.table, other, temp) =>
      director.setStatusBarMessage("Alt+A to set source on effect panel; Alt+M mark selected combatant")
    case FocusLost(this.table, other, temp) =>
      director.setStatusBarMessage("")
  }

  protected object CombatantAtCellDrop {
    def unapply(cell: CellDrop): Option[(UnifiedCombatant, AnyRef)] = {
      Some(table.content(cell.row), cell.data)
    }
  }

  def combatStateChanged(newState: UnifiedSequenceTable) {
    state = newState
    updateContent()

    if (state.orderFirstId.isDefined)
      updateActingCombatantLater(state.orderFirstId)
  }

  override def changeTargetContext(newContext: Option[UnifiedCombatantID]) {
    target = newContext
  }

  override def changeSourceContext(newContext: Option[UnifiedCombatantID]) {
    val oldContext = source
    source = newContext
    table.labelFormatter.updateActing(newContext)
    if (oldContext != newContext) table.repaint()
  }

  private def updateTargetSelectionLater(some: Option[UnifiedCombatantID]) {
    SwingHelper.invokeLater {
      director.setTargetCombatant(some)
    }
  }

  private def updateActingCombatantLater(combatant: Option[UnifiedCombatantID]) {
    SwingHelper.invokeLater {
      director.setActiveCombatant(combatant)
    }
  }

  private def updateContent() {
    mustPreventUpdateOfSelection = true
    table.content = state.elements
    table.labelFormatter.updateNextUp(state.orderFirst)
    updateTableRowSelection(state.elements)
    mustPreventUpdateOfSelection = false

    def updateTableRowSelection(content: Array[UnifiedCombatant]) {
      if (content.isEmpty)
        updateTargetSelectionLater(None)
      else
        updateRowAndTargetSelection(getTargetTableIndexIfDefined, content)
    }

    def updateRowAndTargetSelection(targetIndex: Option[Int], content: Array[UnifiedCombatant]): Any = {
      if (targetIndex.isDefined)
        table.selection.rows += (targetIndex.get)
      else {
        selectDefaultRowAndUpdateTrackerSelection(if (content.length > 1) 1 else 0)
      }
    }

    def selectDefaultRowAndUpdateTrackerSelection(defaultRow: Int) {
      table.selection.rows += defaultRow
      updateTargetSelectionLater(Some(state.elements(defaultRow).unifiedId))
    }

    def getTargetTableIndexIfDefined: Option[Int] = {
      target.flatMap(state.indexOf)
    }
  }

  private def initializeKeyBindings(): Boolean = {
    KeystrokeBinder.bindKeystrokeAction(table, false, KeystrokeBinder.FocusCondition.WhenAncestorFocused, "alt A", setAction)
    KeystrokeBinder.bindKeystrokeAction(table, false, "alt M", Action("sequence.mark") {
      if (target.isDefined && source.isDefined && source.get.orderId != null) {
        director.requestAction(AddEffect(target.get.combId, source.get.combId,
          Effect.Condition.Mark(source.get.combId, false),
          Duration.RoundBound(source.get.orderId, Duration.Limit.EndOfNextTurn)))
      }
    })
    KeystrokeBinder.unbindKeystroke(table, false, KeystrokeBinder.FocusCondition.WhenAncestorFocused, "F2")
    KeystrokeBinder.unbindKeystroke(table, false, KeystrokeBinder.FocusCondition.WhenAncestorFocused, "F8")
  }

  private def initializeDragAndDrop() {
    val drop = new TableCellDropTransferHandler()
    drop.decorateTable(table)
    drop.interestedIn(UnifiedCombatantActionTransfer.UnifiedCombatantDataFlavor) {
      case CombatantAtCellDrop(uc, UnifiedCombatantActionTransfer(_, pf)) if (pf.isDefinedAt(uc)) =>
        pf.apply(uc)
        true
    }
  }
}