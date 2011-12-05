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
package vcc.dnd4e.view.dialog

import java.awt.Window
import vcc.util.swing.ModalPromptDialog
import swing.{Component, Button}
import vcc.util.swing.MigPanel
import java.lang.String
import org.exnebula.swing.{PromptPanelEditListener, JPromptPanelList, PromptPanel}
import vcc.dnd4e.view.IconLibrary

object PromptDialog {

  trait Model {
    def dialogTitle: String

    def prompts: List[PromptPanel]
  }

  case class StaticModel(override val dialogTitle: String, override val prompts: List[PromptPanel]) extends Model

  def promptUserAndDismiss(model: Model): Boolean = {
    val dialog = new PromptDialog(model, null)
    dialog.placeOnScreenCenter()
    dialog.promptUser()
    dialog.dispose()
    dialog.dialogResult.getOrElse(false)
  }
}

class PromptDialog(model: PromptDialog.Model, owner: Window)
  extends ModalPromptDialog[Boolean](owner, "") {

  private val promptPanel = new JPromptPanelList()
  private val okButton = new Button(okAction)

  initializePanel()

  contents = new MigPanel("fill", "[growprio 0]10[growprio 0]10[]", "[growprio 100]10[growprio 0]") {
    add(new Component {
      override lazy val peer = promptPanel
    }, "span 3,width 500, height 165, growx, growy, wrap")
    add(okButton, "width button!")
    add(new Button(cancelAction), "width button!")
  }

  def collectResult(): Option[Boolean] = Option(true)

  private def initializePanel() {
    peer.setTitle(model.dialogTitle)
    addPromptPanelsAndSetActive(model.prompts)
    promptPanel.setRowHeight(50)
    registerEditCompletionListener()
    peer.getRootPane.setDefaultButton(okButton.peer)
    peer.setIconImage(IconLibrary.MetalD20.getImage)

    def registerEditCompletionListener() {
      promptPanel.setAutoSelectNextUnanswered()
      promptPanel.setPromptPanelEditListener(new PromptPanelEditListener {
        def editComplete(panel: Int) {
          okAction.enabled = !promptPanel.hasUnansweredPrompts
        }
      })
    }
  }

  private def addPromptPanelsAndSetActive(prompts: List[PromptPanel]) {
    prompts.foreach(panel => promptPanel.addPromptPanel(panel))
    if (!prompts.isEmpty) {
      promptPanel.setActivePrompt(0)
      okAction.enabled = false
    }
  }
}