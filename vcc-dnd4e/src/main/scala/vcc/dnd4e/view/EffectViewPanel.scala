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
package vcc.dnd4e.view

import scala.swing._
import vcc.util.swing._

import vcc.dnd4e.tracker.common.{Effect}
import vcc.dnd4e.domain.tracker.common.Command._
import vcc.infra.docking._
import vcc.dnd4e.domain.tracker.snapshot.{StateChange}

class EffectViewPanel(director: PanelDirector, isTarget: Boolean) extends MigPanel("fill,ins 2")
with ContextObserver with ScalaDockableComponent with CombatStateObserver {
  private val sustainButton = new Button("Sustain")
  sustainButton.enabled = false

  private val cancelButton = new Button("Cancel Effect")
  cancelButton.enabled = false

  private var context: Option[UnifiedCombatantID] = None

  private var state = director.currentState

  val dockTitle = if (isTarget) "Effect on Target" else "Effect on Source"

  val dockID = DockID(if (isTarget) "tgt-effects" else "src-effects")

  val effectTable = new RowProjectionTable[Effect]() with CustomRenderedRowProjectionTable[Effect] {
    val labelFormatter = tabular.EffectTableColorer
    projection = new vcc.util.swing.ProjectionTableModel[Effect](new tabular.EffectTableProjection(director))
    autoResizeMode = Table.AutoResizeMode.Off
    selection.intervalMode = Table.IntervalMode.Single
    setColumnWidth(0, 25)
    setColumnWidth(1, 50, 50, 100)
    setColumnWidth(2, 200)
  }

  val dockFocusComponent = effectTable.peer

  add(new ScrollPane(effectTable), "growy,growprio 100,h 50:150,wrap")
  add(sustainButton, "split 3,growprio 0")
  add(cancelButton)

  listenTo(effectTable.selection)
  listenTo(sustainButton, cancelButton)
  KeystrokeBinder.bindKeystrokeAction(effectTable, false, KeystrokeBinder.FocusCondition.WhenFocused, javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_DELETE, 0),
    Action(dockID.name + ".cancel") {
      cancelButton.doClick
      effectTable.requestFocus()
    })
  KeystrokeBinder.unbindKeystroke(effectTable, false, KeystrokeBinder.FocusCondition.WhenAncestorFocused, "F2")
  KeystrokeBinder.unbindKeystroke(effectTable, false, KeystrokeBinder.FocusCondition.WhenAncestorFocused, "F8")

  reactions += {
    case event.ButtonClicked(this.sustainButton) =>
      director requestAction SustainEffect(selectedEffectID)
    case event.ButtonClicked(this.cancelButton) =>
      director requestAction CancelEffect(selectedEffectID)
    case event.TableRowsSelected(this.effectTable, rng, opt) =>
      val sel = effectTable.selection.rows
      if (sel.isEmpty) {
        cancelButton.enabled = false
        sustainButton.enabled = false
      } else {
        val eff = effectTable.content(sel.toSeq(0))
        sustainButton.enabled = eff.sustainable
        cancelButton.enabled = true
      }
  }

  private def selectedEffectID() = effectTable.content(effectTable.selection.rows.toSeq(0)).effectId

  /**
   * Update table according to context
   */
  def changeContext(nctx: Option[UnifiedCombatantID], isTarget: Boolean) {
    if (this.isTarget == isTarget) {
      context = nctx
      updateTable()
    }
  }

  private def updateTable() {
    state.combatantOption(context) match {
      case Some(cmb) =>
        effectTable.content = cmb.effects.effects
      case None =>
        effectTable.content = Nil
    }
  }

  def combatStateChanged(newState: UnifiedSequenceTable, changes: StateChange) {
    state = newState
    updateTable()
  }
}
