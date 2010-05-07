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
import vcc.dnd4e.domain.tracker.snapshot.StateChange
import vcc.dnd4e.domain.tracker.common.Command.SetCombatComment

class CombatCommentPanel(director: PanelDirector) extends CommentPanel with CombatStateObserver {
  editorEnabled = true

  def combatStateChanged(newState: UnifiedSequenceTable, changes: StateChange) {
    if (changes.changes.contains(StateChange.combat.MetaData)) {
      updateText(newState.state.combatComment)
    }
  }

  def sendChangeMessage(text: String) {
    director requestAction SetCombatComment(text)
  }

  def dockTitle: String = "Combat Notes"

  def dockID: DockID = DockID("combat-notes")
}