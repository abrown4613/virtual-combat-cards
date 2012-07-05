/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.compendium.view

import scala.swing._
import vcc.util.swing.{MigPanel, SwingHelper}
import vcc.dnd4e.compendium._

class CompendiumView(icon: Image) extends Frame {

  private val window = this
  private val entListPanel = new CompendiumEntitySelectionPanel()

  title = "Compendium Entries"
  iconImage = icon

  val newEntryAction = Action("New Entry ...") {
    val dialog = new NewCombatantDialog(window)
    val result = dialog.promptUser()
    if (result.isDefined)
      doEditEntry(result.get)
  }

  private val editAction = Action("Edit ...") {
    if (entListPanel.currentSelection.isDefined) {
      val ent = Compendium.activeRepository.load(entListPanel.currentSelection.get.eid, false)
      if (ent != null) doEditEntry(ent)
    }
  }

  entListPanel.doubleClickAction = editAction

  contents = new MigPanel("fill, ins dialog") {
    add(entListPanel, "span 5, wrap, growx")
    add(new Button(newEntryAction), "")
    add(new Button(editAction))
    add(new Button(Action("Delete") {
      if (entListPanel.currentSelection.isDefined) {
        Compendium.activeRepository.delete(entListPanel.currentSelection.get.eid)
      }
    }), "")

    add(new Button(Action("Copy") {
      if (entListPanel.currentSelection.isDefined) {
        val ent = Compendium.activeRepository.load(entListPanel.currentSelection.get.eid, false)
        val newCopy = ent.copyEntity()
        Compendium.activeRepository.store(newCopy)
      }
    }), "")

    add(new Button(Action("Close") {
      window.visible = false
    }), "tag right, gap 0")
  }

  def doEditEntry(ent: CombatantEntity) {
    val nd = new CombatantEditorDialog(ent, icon)
    SwingHelper.centerFrameOnScreen(nd)
    nd.visible = true
  }
}