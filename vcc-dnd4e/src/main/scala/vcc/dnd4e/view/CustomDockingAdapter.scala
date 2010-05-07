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

import vcc.infra.docking._
import vcc.infra.docking.idw.InfoNodeDockAdapter
import net.infonode.docking.util.DockingUtil
import net.infonode.docking._
import net.infonode.util.Direction
import scala.swing.Dialog
import vcc.util.swing.SwingHelper

class CustomDockingAdapter extends InfoNodeDockAdapter() {
  def setup(owner: java.awt.Window): javax.swing.JComponent = {
    root = DockingUtil.createRootWindow(vm, true)
    val theme = new net.infonode.docking.theme.SlimFlatDockingTheme()
    root.getRootWindowProperties().addSuperObject(theme.getRootWindowProperties())
    root.getWindowBar(Direction.DOWN).setEnabled(true)
    root.getWindowBar(Direction.LEFT).setEnabled(true)
    root.getWindowBar(Direction.UP).setEnabled(true)
    root.getWindowBar(Direction.RIGHT).setEnabled(true)
    root.getRootWindowProperties.getWindowAreaProperties.setBackgroundColor(root.getWindowBar(Direction.DOWN).getWindowBarProperties.getComponentProperties.getBackgroundColor)
    root.getRootWindowProperties.getSplitWindowProperties.setDividerSize(2)
    //USE THIS TO VIEW LAYOUT:
    //util.DeveloperUtil.createWindowLayoutFrame("My Main RootWindow", root).setVisible(true)
    root
  }

  def restoreDefaultLayout() {
    root.setWindow(new SplitWindow(true, 0.34f,
      new SplitWindow(false, 0.180f,
        vm.getView("damage"),
        new SplitWindow(false, 0.2434f,
          vm.getView("initiative"),
          vm.getView("effect-editor"))),
      new SplitWindow(true, 0.607f,
        new SplitWindow(false, 0.75f,
          vm.getView("sequence"),
          vm.getView("combat-notes")),
        new SplitWindow(false, 0.53f,
          vm.getView("tgt-block"),
          new SplitWindow(false, 0.57f,
            vm.getView("tgt-effects"),
            vm.getView("tgt-notes")
            )))))
    root.getWindowBar(net.infonode.util.Direction.LEFT).addTab(vm.getView("src-block"))
    root.getWindowBar(net.infonode.util.Direction.LEFT).addTab(vm.getView("src-effects"))
    root.getWindowBar(net.infonode.util.Direction.LEFT).addTab(vm.getView("src-notes"))
  }

  //Helper functions to handle layout
  //These are all custom methods (not part of API)

  def getDockLayoutFile(): java.io.File = {
    new java.io.File(vcc.dnd4e.Configuration.baseDirectory.value, "layout.dat")
  }

  def restoreLayoutFromFile(owner: scala.swing.Component) {
    val file = getDockLayoutFile()
    if (file.exists) {
      try {
        restoreLayout(new java.io.FileInputStream(file))
      } catch {
        case s =>
          s.printStackTrace()
          Dialog.showMessage(owner, "Failed to load " + file + ".\nThe file may be corrupt or invalid. Try saving the layout once more.", "Failed to load Layout", Dialog.Message.Error, null)
      }
    } else {
      Dialog.showMessage(owner, "No layout was found, please save prior to attempting a load.", "No layout found", Dialog.Message.Warning, null)
    }
  }

  def storeLayoutToFile(owner: scala.swing.Component) {
    val file = getDockLayoutFile()
    if (!file.exists ||
            Dialog.showConfirmation(owner, "A layout file already exists, overwrite?", "Overwrite layout file?", Dialog.Options.YesNo) == Dialog.Result.Yes
    ) {
      try {
        storeLayout(new java.io.FileOutputStream(file))
      } catch {
        case s =>
          Dialog.showMessage(owner, "Failed to save " + file + ".\nReason: " + s.getMessage, "Failed to save Layout", Dialog.Message.Error, null)
      }
    }
  }

  def loadLayoutOrDefault() {
    SwingHelper.invokeLater {
      val file = getDockLayoutFile
      if (file.exists) {
        try {
          restoreLayout(new java.io.FileInputStream(file))
        } catch {
          case _ => this.restoreDefaultLayout()
        }
      } else
        this.restoreDefaultLayout()
    }
  }

}
