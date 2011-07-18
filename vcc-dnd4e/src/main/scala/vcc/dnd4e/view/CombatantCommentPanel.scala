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

import vcc.infra.docking.DockID
import vcc.dnd4e.domain.tracker.common.Command.SetComment
import vcc.dnd4e.domain.tracker.snapshot.StateChange

class CombatantCommentPanel(director: PanelDirector, isTarget: Boolean) extends CommentPanel with ContextObserver with CombatStateObserver {
  val dockTitle = if (isTarget) "Target Notes" else "Source Notes"

  val dockID = if (isTarget) DockID("tgt-notes") else DockID("src-notes")

  private var context: Option[UnifiedCombatantID] = None

  private var state = director.currentState


  def sendChangeMessage(text: String) {
    director requestAction SetComment(context.get.combId, text)
  }

  def changeContext(nctx: Option[UnifiedCombatantID], isTarget: Boolean) {
    if (this.isTarget == isTarget) {
      if (hasChanged) sendChange()
      context = nctx
      updateCombatant(nctx)
      editorEnabled = context != None
    }

  }

  private def updateCombatant(nctx: Option[UnifiedCombatantID]) {
    val comb = state.combatantOption(context)
    updateText(if (comb.isDefined) comb.get.comment else "")
  }

  def combatStateChanged(newState: UnifiedSequenceTable, changes: StateChange) {
    state = newState
    if (context.isDefined && changes.changesTo(context.get.combId).contains(StateChange.combatant.Comment)) {
      updateCombatant(context)
    }
  }

}