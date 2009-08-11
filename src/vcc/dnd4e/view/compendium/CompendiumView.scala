//$Id$
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

package vcc.dnd4e.view.compendium

import scala.swing._
import scala.swing.event._
import vcc.util.swing.MigPanel

object CompendiumView extends Frame {
  
  title = "Compendium Entries"
  iconImage = IconLibrary.MetalD20.getImage
  
  val window = this

  val newEntryAction = Action("New Entry ...") {
	val diag = new NewCombatantDialog(window)
	diag.visible = true
	println("Dialog is done"+diag.dialogResult)
  }

  contents = new MigPanel("fill") {
    add(new CompendiumEntitySelectionPanel(),"span 3,wrap")
	add(new Button(newEntryAction), "split 4")
	add(new Button(Action("Edit ..."){ doEditEntry() }))
	add(new Button(Action("Close") {CompendiumView.visible = false }))
  }
  
  
  def doEditEntry() {
    
  }
}
