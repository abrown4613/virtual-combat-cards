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
import util.swing._

import vcc.dnd4e.model.common.{Effect}
import vcc.dnd4e.controller.request._
import vcc.infra.docking._
import vcc.dnd4e.model.{CombatState, CombatStateObserver, CombatStateChanges}

class EffectViewPanel(director: PanelDirector, isTarget: Boolean) extends MigPanel("fill,ins 2")
        with ContextObserver with ScalaDockableComponent with CombatStateObserver
{
  private val sustainButton = new Button("Sustain")
  sustainButton.enabled = false

  private val cancelButton = new Button("Cancel Effect")
  cancelButton.enabled = false

  private var context: Option[Symbol] = None

  private var state = director.currentState

  val dockTitle = if (isTarget) "Effect on Target" else "Effect on Source"

  val dockID = DockID(if (isTarget) "tgt-effects" else "src-effects")

  val effectTable = new RowProjectionTable[(Symbol, Int, Effect)]() with CustomRenderedRowProjectionTable[(Symbol, Int, Effect)] {
    val labelFormatter = tabular.EffectTableColorer
    projection = new vcc.util.swing.ProjectionTableModel[(Symbol, Int, Effect)](new tabular.EffectTableProjection(director))
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
      director requestAction SustainEffect(context.get, effectTable.selection.rows.toSeq(0))
    case event.ButtonClicked(this.cancelButton) =>
      director requestAction CancelEffect(context.get, effectTable.selection.rows.toSeq(0))
    case event.TableRowsSelected(this.effectTable, rng, opt) =>
      val sel = effectTable.selection.rows
      if (sel.isEmpty) {
        cancelButton.enabled = false
        sustainButton.enabled = false
      } else {
        val eff = effectTable.content(sel.toSeq(0))
        sustainButton.enabled = eff._3.sustainable
        cancelButton.enabled = true
      }
  }

  /**
   * Update table according to context
   */
  def changeContext(nctx: Option[Symbol], isTarget: Boolean) {
    if (this.isTarget == isTarget) {
      context = nctx
      updateTable()
    }
  }

  private def updateTable() {
    state.getCombatant(context) match {
      case Some(cmb) =>
        effectTable.content = (0 to cmb.effects.length - 1).map(pos => (cmb.id, pos, cmb.effects(pos)))
      case None =>
        effectTable.content = Nil
    }
  }

  def combatStateChanged(newState: CombatState, changes: CombatStateChanges) {
    state = newState
    updateTable()
  }
}
