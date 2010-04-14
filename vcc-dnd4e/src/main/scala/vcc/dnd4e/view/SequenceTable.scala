/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.view

import scala.swing._
import scala.swing.event._
import vcc.util.swing._
import vcc.infra.docking._
import vcc.dnd4e.model.{CombatStateObserver, CombatState, CombatStateChanges, CombatantState}
import tabular._

class SequenceTable(director: PanelDirector) extends ScrollPane
        with ContextObserver with CombatStateObserver with ScalaDockableComponent with PaneDirectorPropertyObserver {
  //Init
  val table = new RowProjectionTable[CombatantState] with CustomRenderedRowProjectionTable[CombatantState] {
    val labelFormatter = new CombatantStateTableColorer(None)
    projection = new ProjectionTableModel[CombatantState](view.tabular.CombatantStateProjection)
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

  private var source: Option[Symbol] = None
  private var target: Option[Symbol] = None
  private var state = director.currentState
  // This variable is used to block target updates by table.seletion listener
  private var _changingState = false

  this.contents = table
  val setAction = Action("sequence.setacting") {
    director.setActiveCombatant(target)
  }
  KeystrokeBinder.bindKeystrokeAction(table, false, KeystrokeBinder.FocusCondition.WhenAncestorFocused, "alt A", setAction)
  KeystrokeBinder.bindKeystrokeAction(table, false, "alt M", Action("sequence.mark") {
    if (target.isDefined && source.isDefined) {
      import vcc.dnd4e.model.common._
      director.requestAction(vcc.dnd4e.controller.request.AddEffect(target.get,
        Effect(source.get, Condition.Mark(source.get, false), false,
          Effect.Duration.RoundBound(source.get, Effect.Duration.Limit.EndOfNextTurn, false))))
    }
  })
  KeystrokeBinder.unbindKeystroke(table, false, KeystrokeBinder.FocusCondition.WhenAncestorFocused, "F2")
  KeystrokeBinder.unbindKeystroke(table, false, KeystrokeBinder.FocusCondition.WhenAncestorFocused, "F8")

  listenTo(table.selection, table)

  reactions += {
    case TableRowsSelected(t, rng, false) if (!_changingState) =>
      var l = table.selection.rows.toSeq
      if (!l.isEmpty) {
        var c = table.content(l(0))
        director.setTargetCombatant(Some(c.id))
      }
    case FocusGained(this.table, other, true) =>
      director.setStatusBarMessage("Alt+A to set source on effect panel; Alt+M mark selected combatant")
    case FocusLost(this.table, other, true) =>
      director.setStatusBarMessage("")
  }

  def combatStateChanged(newState: CombatState, changes: CombatStateChanges) {
    state = newState
    if (
      changes.changes.contains(CombatState.part.Sequence) ||
              !changes.combatantsThatChange(CombatantState.part.Health).isEmpty ||
              !changes.combatantsThatChange(CombatantState.part.Initiative).isEmpty
    ) {
      updateContent()
      //On a sequence change
      if (changes.changes.contains(CombatState.part.Sequence)) {
        val newfirst = if (table.content.isEmpty) None else Some(table.content(0).id)
        if (newfirst != source) director.setActiveCombatant(newfirst)
      }
    }
  }

  private def updateContent() {
    import vcc.dnd4e.model.common.HealthTracker
    import vcc.dnd4e.model.common.InitiativeState
    _changingState = true
    val hidedead = director.getBooleanProperty(PanelDirector.property.HideDead)
    val ncontent = if (hidedead && !state.combatantSequence.isEmpty) {
      val l = state.combatantSequence.toList
      l.head :: l.tail.filter(x => x.health.status != HealthTracker.Status.Dead)
    } else state.combatantSequence

    table.content = ncontent

    //Adjust selection
    if (ncontent.length > 0) {
      val idx: Int = { // -1 means not found
        val obj = if (target.isDefined) ncontent.find(x => x.id == target.get) else None
        if (obj.isDefined) ncontent.indexOf(obj.get) else -1
      }
      if (idx == -1) { //Select first as active and second, if present as target
        val defaultRow = if (ncontent.length > 1) 1 else 0
        table.selection.rows += defaultRow

        //This has to fire later to make sure everyone gets the state update first.
        SwingHelper.invokeLater {director.setTargetCombatant(Some(ncontent(defaultRow).id))}
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

  def changeContext(nctx: Option[Symbol], isTarget: Boolean) {
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
  val dockTitle = "Combat Sequence"
  val dockFocusComponent = table.peer
}
