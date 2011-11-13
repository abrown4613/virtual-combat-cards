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
with ContextObserver with CombatStateObserver with ScalaDockableComponent with PaneDirectorPropertyObserver {
  //Init
  val table = new RowProjectionTable[UnifiedCombatant] with CustomRenderedRowProjectionTable[UnifiedCombatant] {
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

  private var source: Option[UnifiedCombatantID] = None
  private var target: Option[UnifiedCombatantID] = None
  private var state = director.currentState
  // This variable is used to block target updates by table.seletion listener
  private var _changingState = false

  this.contents = table
  val setAction = Action("sequence.setacting") {
    director.setActiveCombatant(target)
  }
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

  listenTo(table.selection, table)

  reactions += {
    case TableRowsSelected(t, rng, false) if (!_changingState) =>
      val l = table.selection.rows.toSeq
      if (!l.isEmpty) {
        val c = table.content(l(0))
        director.setTargetCombatant(Some(c.unifiedId))
      }
    case FocusGained(this.table, other, temp) =>
      director.setStatusBarMessage("Alt+A to set source on effect panel; Alt+M mark selected combatant")
    case FocusLost(this.table, other, temp) =>
      director.setStatusBarMessage("")
  }

  //Enable drag and drop
  protected object CombatantAtCellDrop {
    def unapply(d: CellDrop): Option[(UnifiedCombatant, AnyRef)] = {
      Some(table.content(d.row), d.data)
    }
  }

  val cellDrop = new TableCellDropTransferHandler()

  cellDrop.decorateTable(table)
  cellDrop.interestedIn(UnifiedCombatantActionTransfer.UnifiedCombatantDataFlavor) {
    case CombatantAtCellDrop(uc, UnifiedCombatantActionTransfer(_, pf)) if (pf.isDefinedAt(uc)) =>
      pf.apply(uc)
      true
  }

  def combatStateChanged(newState: UnifiedSequenceTable) {
    state = newState
    updateContent()

    SwingHelper.invokeLater {
      director.setActiveCombatant(state.orderFirstId)
    }
  }

  private def updateContent() {
    _changingState = true
    val hideDead = director.getBooleanProperty(PanelDirector.property.HideDead)
    val ncontent = if (hideDead && !state.elements.isEmpty) {
      state.elements.filter(c => c.health.status != HealthStatus.Dead || (state.orderFirst.isDefined && c.matches(state.orderFirst.get)))
    } else state.elements

    table.content = ncontent

    table.labelFormatter.updateNextUp(state.orderFirst)
    //Adjust selection
    if (!ncontent.isEmpty) {
      val idx: Int = {
        // -1 means not found
        val obj = if (target.isDefined) ncontent.find(x => x.matches(target.get)) else None
        if (obj.isDefined) ncontent.indexOf(obj.get) else -1
      }
      if (idx == -1) {
        //Select first as active and second, if present as target
        val defaultRow = if (ncontent.length > 1) 1 else 0
        table.selection.rows += defaultRow

        //This has to fire later to make sure everyone gets the state update first.
        SwingHelper.invokeLater {
          director.setTargetCombatant(Some(ncontent(defaultRow).unifiedId))
        }
      } else {
        //Just show the correct selection
        table.selection.rows += (idx)
      }
    } else {
      //Nothing to show set context accordingly
      director.setTargetCombatant(None)
    }
    _changingState = false
  }

  def changeContext(nctx: Option[UnifiedCombatantID], isTarget: Boolean) {
    if (isTarget) {
      target = nctx
    } else {
      val oldCtx = source
      source = nctx
      table.labelFormatter.updateActing(nctx)
      if (oldCtx != nctx) table.repaint()
    }
  }

  def propertyChanged(which: PanelDirector.property.Value) {
    if (which == PanelDirector.property.HideDead) {
      updateContent()
    }
  }

  val dockID = DockID("sequence")
  val dockTitle = "Initiative Order"
  val dockFocusComponent = table.peer
}
