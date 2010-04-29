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

import swing._
import event._
import javax.swing.BorderFactory
import vcc.dnd4e.domain.tracker.common.Command.SetComment
import vcc.util.swing.MigPanel
import vcc.infra.docking.{DockableComponent, DockID}
import vcc.dnd4e.domain.tracker.snapshot.{CombatantState, CombatState, StateChange}
import vcc.dnd4e.domain.tracker.common.CombatantID

class CommentPanel(director: PanelDirector, isTarget: Boolean) extends MigPanel("fill,ins 0", "", "")
        with ContextObserver with CombatStateObserver with DockableComponent {
  private var _hasChanged = false
  private var _updating = false
  private val edit = new TextArea {
    enabled = false
  }

  private var context: Option[CombatantID] = None

  private var state: CombatState = director.currentState

  xLayoutAlignment = java.awt.Component.LEFT_ALIGNMENT;

  val dockTitle = if (isTarget) "Target Notes" else "Source Notes"

  val dockRootComponent = this.peer

  val dockFocusComponent = edit.peer

  val dockID = if (isTarget) DockID("tgt-notes") else DockID("src-notes")

  @deprecated
  val debug = new Label(context.toString)

  add(new ScrollPane {
    border = BorderFactory.createLoweredBevelBorder
    contents = edit
  }, "growx,growy")

  listenTo(edit)
  reactions += {
    case FocusLost(edit: TextArea, opt, temp) if (_hasChanged) =>
      sendChange()
    case ValueChanged(edit) =>
      if (!_updating) _hasChanged = true
  }

  private def sendChange() {
    if (_hasChanged) {
      _hasChanged = false
      director requestAction SetComment(context.get, edit.text)
    }
  }

  def changeContext(nctx: Option[CombatantID], isTarget: Boolean) {
    if (this.isTarget == isTarget) {
      if (_hasChanged) sendChange()
      context = nctx
      updateCombatant(nctx)
      edit.enabled = context != None
    }

  }

  private def updateCombatant(nctx: Option[CombatantID]) {
    _updating = true
    //TODO Move to simpler in unified combatant
    edit.text = if (context.isDefined && state.roster.isDefinedAt(context.get)) state.combatantViewFromID(context.get).comment else ""
    _updating = false
  }

  def combatStateChanged(newState: CombatState, uv: Array[UnifiedCombatant], changes: StateChange) {
    state = newState
    //TODO Move to simpler in unified combatant
    if (context.isDefined && changes.changesTo(context.get).contains(StateChange.combatant.Comment)) {
      updateCombatant(context)
    }
  }
}
