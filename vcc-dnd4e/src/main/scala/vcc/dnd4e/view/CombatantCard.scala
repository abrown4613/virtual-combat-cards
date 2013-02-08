/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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

import scala.swing._
import vcc.util.swing._
import helper.CombatantStatBlockCache
import vcc.infra.docking._
import java.awt.Dimension
import vcc.dnd4e.tracker.common.UnifiedCombatantID

abstract class CombatantCard(director: PanelDirector)
  extends GridPanel(1, 1) with ContextObserver with SimpleCombatStateObserver with ScalaDockableComponent {

  private val statBlock = new XHTMLPane
  statBlock.minimumSize = new Dimension(200, 200)
  contents += statBlock

  minimumSize = new Dimension(300, 400)

  val dockFocusComponent = statBlock.peer

  protected def updateStatBlock(context: Option[UnifiedCombatantID]) {
    val cmb = combatState.combatantOption(context)
    if (cmb.isDefined) {
      statBlock.setDocument(CombatantStatBlockCache.getStatBlockDocumentForCombatant(cmb.get.definition.entity.eid, cmb.get.definition.entity.statBlock))
    }
    else statBlock.setDocumentFromText("")
  }
}

class TargetCombatantCard(director: PanelDirector) extends CombatantCard(director) {
  val dockTitle = "Target"

  val dockID = DockID("tgt-block")

  override def changeTargetContext(newContext: Option[UnifiedCombatantID]) {
    updateStatBlock(newContext)
  }
}

class SourceCombatantCard(director: PanelDirector) extends CombatantCard(director) {
  val dockTitle = "Source"

  val dockID = DockID("src-block")

  override def changeSourceContext(newContext: Option[UnifiedCombatantID]) {
    updateStatBlock(newContext)
  }
}