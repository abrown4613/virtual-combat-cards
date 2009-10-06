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
package vcc.dnd4e.view

import scala.swing._
import scala.actors.Actor

import vcc.model.Registry
import vcc.dnd4e.view.dialog.FileChooserHelper
import vcc.dnd4e.model.PartyLoader
import vcc.infra.datastore.naming.DataStoreURI
import vcc.dnd4e.controller.request._
import vcc.util.swing.SwingHelper
import vcc.dnd4e.view.compendium.CompendiumMenu
import vcc.controller.actions.ClearTransactionLog

class MainMenu(uia:Actor) extends MenuBar {
  
  val compendiumID = Registry.get[DataStoreURI]("Compendium").get
  val tracker = Registry.get[Actor]("tracker").get
  
  var fileMenu=new Menu("File");
  fileMenu.contents += new MenuItem(Action("Load Party ..."){
    var file=FileChooserHelper.chooseOpenFile(this.peer,FileChooserHelper.partyFilter)
    if(file.isDefined) {
      val pml = PartyLoader.loadFromFile(compendiumID,file.get)
      PartyLoader.loadToBattle(compendiumID,pml)                                   
    }
  })
  fileMenu.contents += new Separator()
  fileMenu.contents += new MenuItem(Action("Preferences ..."){
    var cdiag = new vcc.dnd4e.ConfigurationDialog(null,false)
    cdiag.visible = true
  })
  
  val combatMenu = new Menu("Combat")
  combatMenu.contents +=new MenuItem(new Action("Go to First"){
    def apply():Unit={
      uia ! vcc.dnd4e.view.actor.GoToFirst()
    }
    accelerator=Some(javax.swing.KeyStroke.getKeyStroke('F'.toInt,java.awt.Event.CTRL_MASK))
  })
  combatMenu.contents += new Separator
  combatMenu.contents += new MenuItem(Action("Start Combat") {
    val diag=new vcc.dnd4e.view.dialog.InitiativeDialog(tracker)
    diag.visible=true
  })
  combatMenu.contents += new MenuItem(Action("End Combat") {
    tracker ! EndCombat()
    tracker ! Enumerate()
  })
  combatMenu.contents += new MenuItem(Action("Rest") {
    tracker ! ApplyRest(false)
  })
  combatMenu.contents += new MenuItem(Action("Extended Rest") {
    tracker ! ApplyRest(true)
  })
  
  combatMenu.contents += new Separator
  combatMenu.contents +=new MenuItem(Action("Clear Monsters"){
    tracker ! ClearCombatants(false)
    tracker ! Enumerate()
  })
  combatMenu.contents +=new MenuItem(Action("Clear All"){
    tracker ! ClearCombatants(true)
    tracker ! Enumerate()
  })
  
  val historyMenu= new Menu("History")
  historyMenu.contents +=new MenuItem(new Action("Undo"){
    def apply():Unit={
      tracker ! vcc.controller.actions.Undo()
    }
    accelerator=Some(javax.swing.KeyStroke.getKeyStroke('Z'.toInt,java.awt.Event.CTRL_MASK))
  })
  historyMenu.contents +=new MenuItem(new Action("Redo"){
    def apply():Unit={
      tracker ! vcc.controller.actions.Redo()
    }
    accelerator=Some(javax.swing.KeyStroke.getKeyStroke('Y'.toInt,java.awt.Event.CTRL_MASK))
  })
  historyMenu.contents += new Separator
  historyMenu.contents +=new MenuItem(Action("Clear History"){
    tracker ! ClearTransactionLog()
  })
  
  var viewMenu= new Menu("View")
  var hideDeadMenu=new CheckMenuItem("Hide Dead")
  hideDeadMenu.action=Action("Hide Dead"){
    uia ! vcc.dnd4e.view.actor.SetOption('HIDEDEAD,hideDeadMenu.peer.isSelected)
  }
  viewMenu.contents +=hideDeadMenu
  
  //Help menu
  val helpMenu = new Menu("Help")
  helpMenu.contents += new MenuItem(Action("Online Manual"){
    val dsk=java.awt.Desktop.getDesktop
    dsk.browse(new java.net.URL("http://www.exnebula.org/vcc/manual").toURI)
  })

  helpMenu.contents += new MenuItem(Action("Check for Updates ..."){
    SwingHelper.invokeInOtherThread {
      println("Update manager: Starting update")
      val url=System.getProperty("vcc.update.url","http://www.exnebula.org/files/release-history/vcc/vcc-all.xml")
      println("Update manager: Fetch version from URL: "+url)
      vcc.util.UpdateManager.runUpgradeProcess(new java.net.URL(url))
      println("Update manager: End update")
    }
  })

  helpMenu.contents += new Separator
  helpMenu.contents += new MenuItem(Action("About") {
    Dialog.showMessage(
      this,
      "This is Virtual Combant Cards version: "+ vcc.dnd4e.BootStrap.version.versionString+
        "\nDesigned at: www.exnebula.org",
      "About Virtual Combat Cards",
      Dialog.Message.Info, vcc.dnd4e.view.IconLibrary.MetalD20
    )
  })
  
  contents+=fileMenu
  contents+=combatMenu
  contents+=historyMenu
  contents+=viewMenu
  contents+=new CompendiumMenu()
  contents+=helpMenu
}
