/**
 *  Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
import vcc.dnd4e.view.dialog.FileChooserHelper
import vcc.util.swing.SwingHelper
import vcc.dnd4e.view.compendium.CompendiumMenu
import vcc.controller.message.ClearTransactionLog
import vcc.dnd4e.view.helper.PartyLoader

/**
 * Helper object to create MenuItem associated to PanelDirector properties
 */
object PropertyMenuItem {

  /**
   * Create a CheckMenuItem associated with a boolean PanelDirector property.
   */
  def createCheckMenu(name: String, director: PanelDirector, prop: PanelDirector.property.Value): MenuItem = {
    val mi = new CheckMenuItem(name)
    mi.selected = director.getBooleanProperty(prop)
    mi.action = Action(name) {
      director.setProperty(prop, mi.selected)
    }
    mi
  }
}

class MainMenu(director: PanelDirector, docker: CustomDockingAdapter, parent: Frame) extends MenuBar {
  private val logger = org.slf4j.LoggerFactory.getLogger("user")

  private val fileMenu = new Menu("File");
  fileMenu.contents += new MenuItem(Action("Load Party ...") {
    var file = FileChooserHelper.chooseOpenFile(this.peer, FileChooserHelper.partyFilter)
    if (file.isDefined) {
      PartyLoader.loadToBattle(director, Component.wrap(parent.peer.getRootPane), file.get)
    }
  })
  fileMenu.contents += new Separator()
  fileMenu.contents += new MenuItem(Action("Preferences ...") {
    val cdiag = new vcc.dnd4e.ConfigurationDialog(null, false)
    cdiag.visible = true
  })

  private val combatMenu = new CombatMenu(director, parent)

  private val historyMenu = new Menu("History")
  historyMenu.contents += new MenuItem(new Action("Undo") {
    def apply(): Unit = {
      director requestControllerOperation vcc.controller.message.Undo()
    }
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke('Z'.toInt, java.awt.Event.CTRL_MASK))
  })
  historyMenu.contents += new MenuItem(new Action("Redo") {
    def apply(): Unit = {
      director requestControllerOperation vcc.controller.message.Redo()
    }
    accelerator = Some(javax.swing.KeyStroke.getKeyStroke('Y'.toInt, java.awt.Event.CTRL_MASK))
  })
  historyMenu.contents += new Separator
  historyMenu.contents += new MenuItem(Action("Clear History") {
    director requestControllerOperation ClearTransactionLog()
  })

  private val viewMenu = new Menu("View")
  private val hideDeadMenu = PropertyMenuItem.createCheckMenu("Hide Dead", director, PanelDirector.property.HideDead)
  private val robinViewMenu = PropertyMenuItem.createCheckMenu("Show Next Up as first combatant", director, PanelDirector.property.RobinView)

  viewMenu.contents += hideDeadMenu
  viewMenu.contents += robinViewMenu

  private val dockMenu = new Menu("Dockable")

  private val dockRestoreMenu = new Menu("Restore")
  private val dockFocusMenu = new Menu("Go to window")
  dockMenu.contents += dockRestoreMenu
  dockMenu.contents += dockFocusMenu
  dockMenu.contents += new Separator
  dockMenu.contents += new MenuItem(Action("Restore Default Layout") {docker.restoreDefaultLayout()})
  dockMenu.contents += new MenuItem(Action("Save Layout") {docker.storeLayoutToFile(Component.wrap(parent.peer.getRootPane))})
  dockMenu.contents += new MenuItem(Action("Load Layout") {docker.restoreLayoutFromFile(Component.wrap(parent.peer.getRootPane))})

  def addToDockRestoreMenu(item: MenuItem) {
    dockRestoreMenu.contents += item
  }

  def addToDockRestoreFocusMenu(item: MenuItem) {
    dockFocusMenu.contents += item
  }

  //Help menu
  private val helpMenu = new Menu("Help")
  helpMenu.contents += new MenuItem(Action("Online Manual") {
    val dsk = java.awt.Desktop.getDesktop
    dsk.browse(new java.net.URL("http://www.exnebula.org/vcc/manual").toURI)
  })

  helpMenu.contents += new MenuItem(Action("Firefox plugin") {
    val dsk = java.awt.Desktop.getDesktop
    dsk.browse(new java.net.URL("http://www.exnebula.org/vcc/plugin").toURI)
  })

  helpMenu.contents += new MenuItem(Action("Check for Updates ...") {
    SwingHelper.invokeInOtherThread {
      logger.info("Update manager: Starting update")
      val url = System.getProperty("vcc.update.url", "http://www.exnebula.org/files/release-history/vcc/vcc-all.xml")
      logger.info("Update manager: Fetch version from URL: " + url)
      vcc.util.UpdateManager.runUpgradeProcess(new java.net.URL(url))
      logger.info("Update manager: End update")
    }
  })

  helpMenu.contents += new Separator
  helpMenu.contents += new MenuItem(Action("About") {
    Dialog.showMessage(
      Component.wrap(parent.peer.getRootPane),
      "This is Virtual Combant Cards version: " + vcc.dnd4e.BootStrap.version.versionString +
              "\nDesigned at: www.exnebula.org",
      "About Virtual Combat Cards",
      Dialog.Message.Info, vcc.dnd4e.view.IconLibrary.MetalD20
      )
  })

  contents += fileMenu
  contents += combatMenu
  contents += historyMenu
  contents += viewMenu
  contents += new CompendiumMenu(director)
  contents += dockMenu
  contents += helpMenu
}
