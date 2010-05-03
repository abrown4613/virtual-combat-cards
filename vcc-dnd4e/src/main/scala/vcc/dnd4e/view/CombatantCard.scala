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
import helper.CombatantStatBlockCache
import vcc.infra.docking._
import vcc.dnd4e.domain.tracker.common.CombatantID

class CombatantCard(diretor: PanelDirector, isTarget: Boolean) extends GridPanel(1, 1) with ContextObserver with SimpleCombatStateObserver with ScalaDockableComponent {
  def changeContext(nctx: Option[UnifiedCombatantID], isTarget: Boolean) {
    if (this.isTarget == isTarget) {
      val cmb = combatState.combatantOption(nctx)
      if (cmb.isDefined) {
        statBlock.setDocument(CombatantStatBlockCache.getStatBlockDocumentForCombatant(cmb.get.definition.entity.eid, cmb.get.definition.entity.statBlock))
      }
      else statBlock.setDocumentFromText("")
    }
  }

  minimumSize = new java.awt.Dimension(300, 400)

  private val statBlock = new XHTMLPane
  statBlock.minimumSize = new java.awt.Dimension(200, 200)
  contents += statBlock

  val dockID = DockID(if (isTarget) "tgt-block" else "src-block")
  val dockTitle = if (isTarget) "Target" else "Source"
  val dockFocusComponent = statBlock.peer

}
