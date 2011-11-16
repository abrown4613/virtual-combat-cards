/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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

import ruling.{TranslatorService, RulingDialog}
import scala.swing._
import event.{WindowOpened, WindowClosing}
import vcc.infra.docking._
import vcc.infra.prompter.RulingBroker
import vcc.util.swing.{SwingHelper, KeystrokeContainer}
import vcc.util.UpdateManager.Version
import java.awt.Toolkit
import java.io.File
import java.net.URL
import vcc.dnd4e.tracker.common.CombatState
import vcc.tracker.Tracker
import vcc.dnd4e.tracker.dispatcher.InterimController

case class ReleaseInformation(currentVersion: Version, versionReleaseURL: URL, checkAfterAge: Long)

class MasterFrame(baseDirectory: File, releaseInformation: ReleaseInformation, configurationPanel: ConfigurationPanelCallback)
  extends Frame {

  private val docker = new CustomDockingAdapter(baseDirectory)

  private val statusBar = new StatusBar(releaseInformation.currentVersion)

  private val newTracker = new Tracker[CombatState](new InterimController())

  private val director = new PanelDirector(newTracker, statusBar,
    new RulingBroker(RulingDialog.getInstanceAndController(this), TranslatorService.getInstance()))
  private val news = new NewsPanel(baseDirectory, releaseInformation)
  private val docks = createAllDockableComponents()
  private val mainMenu = new MainMenu(director, docker, this, configurationPanel, releaseInformation)

  adjustPreferredSize()
  registerPanelsWithPanelDirector()
  registerDockableWithDockerAndRegisterKeyBinding()
  initializeAndDecorateFrame()
  registerDockableKeyStroke()
  docker.loadLayoutOrDefault()
  registerReactions()

  SwingHelper.invokeInEventDispatchThread {
    newTracker.initializeState(CombatState.empty)
  }

  private def createAllDockableComponents(): List[DockableComponent] = {
    List[DockableComponent](
      new DamageCommandPanel(director),
      new InitiativePanel(director),
      new EffectEditorPanel(director, getNumberOfEditorPanel),
      new CombatCommentPanel(director),
      new SequenceTable(director),

      new TargetCombatantCard(director),
      new TargetEffectViewPanel(director),
      new TargetCombatantCommentPanel(director),

      new SourceCombatantCard(director),
      new SourceEffectViewPanel(director),
      new SourceCombatantCommentPanel(director),
      news
    )
  }

  private def getNumberOfEditorPanel: Int = {
    def getMaximumNumberOfPanels: Int = try {
      System.getProperty("vcc.view.efp.max").toInt
    } catch {
      case _ => 3
    }

    if (Toolkit.getDefaultToolkit.getScreenSize.getHeight > 700)
      getMaximumNumberOfPanels
    else
      2
  }

  private def adjustPreferredSize() {
    preferredSize = {
      val toolkit = java.awt.Toolkit.getDefaultToolkit;
      val dimension = toolkit.getScreenSize;
      if (dimension != null)
        new java.awt.Dimension(if (dimension.getWidth >= 1150) 1150 else 800, if (dimension.getHeight >= 700) 690 else 600)
      else
        new java.awt.Dimension(800, 600)
    }
  }

  private def registerDockableWithDockerAndRegisterKeyBinding() {
    for ((dock, keystroke) <- docks.zip(List("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "alt F6", "alt F7", "alt F8", null))) {
      docker.addDockable(dock)
      mainMenu.addToDockRestoreMenu(new MenuItem(new DockableRestoreAction(docker, dock.dockID, dock.dockTitle)))
      val fma = new MenuItem(new DockableFocusAction(docker, dock.dockID, dock.dockTitle))
      if (keystroke != null) fma.peer.setAccelerator(javax.swing.KeyStroke.getKeyStroke(keystroke))
      mainMenu.addToDockRestoreFocusMenu(fma)
    }
  }

  private def registerPanelsWithPanelDirector() {
    for (dock <- docks) {
      if (dock.isInstanceOf[ContextObserver]) director.registerContextObserver(dock.asInstanceOf[ContextObserver])
      if (dock.isInstanceOf[CombatStateObserver]) director.registerStateObserver(dock.asInstanceOf[CombatStateObserver])
    }
  }

  private def registerDockableKeyStroke() {
    SwingHelper.invokeLater {
      for (dock <- docks)
        if (dock.isInstanceOf[KeystrokeContainer]) dock.asInstanceOf[KeystrokeContainer].registerKeystroke()
    }
  }

  private def initializeAndDecorateFrame() {
    menuBar = mainMenu
    iconImage = IconLibrary.MetalD20.getImage
    contents = new BorderPanel {
      peer.add(docker.setup(null), java.awt.BorderLayout.CENTER)
      add(statusBar, BorderPanel.Position.South)
      background = java.awt.Color.BLUE
    }
    title = "Virtual Combat Cards"
    peer.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE)
  }

  private def registerReactions() = {
    listenTo(this)

    reactions += {
      case WindowClosing(win) =>
        if (System.getProperty("vcc.quickexit") != null ||
          Dialog.showConfirmation(Component.wrap(this.peer.getRootPane), "Quitting Virtual Combat Cards will mean you loose all the combat information.\nAre you sure?", "Quit?", Dialog.Options.YesNo) == Dialog.Result.Yes) {
          this.dispose()
          System.exit(1)
        }
      case WindowOpened(win) =>
        //Go fetch new and updates if needed
        news.updateIfOld(releaseInformation.checkAfterAge, docker);
    }
  }
}