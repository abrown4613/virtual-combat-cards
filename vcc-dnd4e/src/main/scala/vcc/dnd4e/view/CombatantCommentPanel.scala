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

import vcc.infra.docking.DockID
import vcc.dnd4e.tracker.common.Command.SetComment

abstract class CombatantCommentPanel(director: PanelDirector) extends CommentPanel with ContextObserver with CombatStateObserver {
  private var context: Option[UnifiedCombatantID] = None
  private var state:UnifiedSequenceTable = null

  def sendChangeMessage(text: String) {
    director requestAction SetComment(context.get.combId, text)
  }

  protected def sendChangeAndUpdateControl(nctx: Option[UnifiedCombatantID]) {
    if (hasChanged) sendChange()
    context = nctx
    updateCombatant(nctx)
    editorEnabled = context != None
  }

  private def updateCombatant(nctx: Option[UnifiedCombatantID]) {
    val comb = state.combatantOption(context)
    updateText(if (comb.isDefined) comb.get.comment else "")
  }

  def combatStateChanged(newState: UnifiedSequenceTable) {
    state = newState
    updateCombatant(context)
  }
}

class SourceCombatantCommentPanel(panelDirector:PanelDirector) extends CombatantCommentPanel(panelDirector) {

  val dockTitle = "Source Notes"

  val dockID = DockID("src-notes")

  override def changeSourceContext(newContext: Option[UnifiedCombatantID]) {
    sendChangeAndUpdateControl(newContext)
  }
}

class TargetCombatantCommentPanel(panelDirector:PanelDirector) extends CombatantCommentPanel(panelDirector) {
  val dockTitle = "Target Notes"

  val dockID = DockID("tgt-notes")

  override def changeTargetContext(newContext: Option[UnifiedCombatantID]) {
    sendChangeAndUpdateControl(newContext)
  }
}