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
package vcc.dnd4e.view.compendium

import scala.swing._
import vcc.util.swing.SwingHelper
import java.io.FileInputStream
import vcc.dnd4e.compendium.{CombatantEntityBuilder, Compendium}
import vcc.dndi.reader.CharacterBuilderImporter
import vcc.dnd4e.view.{IconLibrary, PanelDirector}
import vcc.dnd4e.compendium.view.CompendiumView
import vcc.dnd4e.view.dialog.{PartyEditorView, FileChooserHelper}

class CompendiumMenu(director:PanelDirector) extends Menu("Compendium") {

  private val logger = org.slf4j.LoggerFactory.getLogger("user")
  private val compendiumView = new CompendiumView(IconLibrary.MetalD20.getImage)

  this.contents += new MenuItem(Action("Import Character Builder File..."){
    val file=FileChooserHelper.chooseOpenFile(this.peer,FileChooserHelper.characterBuilderFilter)
    if(file.isDefined) {
      try {
        val dse = CharacterBuilderImporter.loadFromStream(new FileInputStream(file.get))
        val ent = CombatantEntityBuilder.buildEntity(dse)
        Compendium.activeRepository.store(ent)
      } catch {
        case e =>
          logger.warn("Failed to load file "+file.get.getAbsolutePath+": reason",e)
          Dialog.showMessage(this, "Failed to load "+ file.get.getAbsoluteFile +".\nThe file may be corrupt or invalid.", "Failed to import file",Dialog.Message.Error,null)
      }
    }
  })

  this.contents += new MenuItem(Action("View Entries ...") {
    SwingHelper.centerFrameOnScreen(compendiumView)
    compendiumView.visible = true
  })

  this.contents += new MenuItem(compendiumView.newEntryAction)

  this.contents += new MenuItem(Action("Edit Parties ...") {
    val partyEditor = new PartyEditorView(director)
    SwingHelper.centerFrameOnScreen(partyEditor)
    partyEditor.visible = true
  })
  this.contents += new MenuItem(Action("D&D Insider Capture ...") {
    DNDICaptureMonitor.getInstance().visible = true
  })
}