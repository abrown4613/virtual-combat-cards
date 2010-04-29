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

import scala.actors.Actor
import scala.swing._
import scala.swing.event.WindowClosing
import vcc.model.Registry
import vcc.infra.docking._
import vcc.util.swing.KeystrokeContainer
import vcc.controller.TrackerChangeObserver
import vcc.dnd4e.domain.tracker.snapshot.{CombatChangeAndStateSnapshotBuilder, CombatStateWithChanges}

class MasterFrame extends Frame {
  val docker = new CustomDockingAdapter()
  val tracker = Registry.get[Actor]("tracker").get

  val statusBar = new StatusBar()
  val csm = new TrackerChangeObserver[CombatStateWithChanges](new CombatChangeAndStateSnapshotBuilder(), tracker)
  val director = new PanelDirector(tracker, csm, statusBar)

  val docks = List[DockableComponent](
    new DamageCommandPanel(director),
    new InitiativePanel(director),
    new EffectEditorPanel(director),
    new vcc.dnd4e.view.SequenceTable(director),
    //Targt panels
    new vcc.dnd4e.view.CombatantCard(director, true),
    new EffectViewPanel(director, true),
    new CommentPanel(director, true),
    // Source Panel
    new vcc.dnd4e.view.CombatantCard(director, false),
    new EffectViewPanel(director, false),
    new CommentPanel(director, false)
    )

  title = "Virtual Combat Cards"

  peer.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE)
  listenTo(this)
  reactions += {
    case WindowClosing(win) =>
      if (System.getProperty("vcc.quickexit") != null ||
              Dialog.showConfirmation(Component.wrap(this.peer.getRootPane), "Quitting Virtual Combat Cards will mean you loose all the combat information.\nAre you sure?", "Quit?", Dialog.Options.YesNo) == Dialog.Result.Yes) {
        this.dispose
        System.exit(1)
      }
  }


  preferredSize = {
    val toolkit = java.awt.Toolkit.getDefaultToolkit();
    val dimension = toolkit.getScreenSize();
    if (dimension != null)
      new java.awt.Dimension(if (dimension.getWidth() >= 1150) 1150 else 800, if (dimension.getHeight() >= 700) 690 else 600)
    else
      new java.awt.Dimension(800, 600)
  }

  //Register Dock with PanelDirector
  for (dock <- docks) {
    if (dock.isInstanceOf[ContextObserver]) director.registerContextObserver(dock.asInstanceOf[ContextObserver])
    if (dock.isInstanceOf[CombatStateObserver]) director.registerStateObserver(dock.asInstanceOf[CombatStateObserver])
    if (dock.isInstanceOf[PaneDirectorPropertyObserver]) director.registerPropertyObserver(dock.asInstanceOf[PaneDirectorPropertyObserver])
  }

  val mainMenu = new MainMenu(director, docker, this)

  for ((dock, keystroke) <- docks.zip(List("F1", "F2", "F3", "F5", "F6", "F7", "F8", "alt F6", "alt F7", "alt F8"))) {
    docker.addDockable(dock)
    mainMenu.addToDockRestoreMenu(new MenuItem(new DockableRestoreAction(docker, dock.dockID, dock.dockTitle)))
    val fma = new MenuItem(new DockableFocusAction(docker, dock.dockID, dock.dockTitle))
    if (keystroke != null) fma.peer.setAccelerator(javax.swing.KeyStroke.getKeyStroke(keystroke))
    mainMenu.addToDockRestoreFocusMenu(fma)
  }

  menuBar = mainMenu

  contents = new BorderPanel {
    peer.add(docker.setup(null), java.awt.BorderLayout.CENTER)
    add(statusBar, BorderPanel.Position.South)
    background = java.awt.Color.BLUE
  }
  iconImage = IconLibrary.MetalD20.getImage()

  //Last operations
  vcc.util.swing.SwingHelper.invokeLater {
    for (dock <- docks)
      if (dock.isInstanceOf[KeystrokeContainer]) dock.asInstanceOf[KeystrokeContainer].registerKeystroke
  }
  docker.loadLayoutOrDefault()

}
