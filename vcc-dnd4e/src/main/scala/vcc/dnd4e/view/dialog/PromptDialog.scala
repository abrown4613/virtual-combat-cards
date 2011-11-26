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
import org.exnebula.swing.JPromptPanelList
import swing.{Component, Button}
import vcc.util.swing.MigPanel
import org.exnebula.swing.PromptPanel
import java.lang.String


object PromptDialog {

  trait Model {
    def dialogTitle: String

    def prompts: List[PromptPanel]
  }

  case class StaticModel(override val dialogTitle: String, override val prompts: List[PromptPanel]) extends Model

}

class PromptDialog(model: PromptDialog.Model, owner: Window)
  extends ModalPromptDialog[Boolean](owner, "") {

  private def initializePanel() {
    peer.setTitle(model.dialogTitle)
    val prompts: List[PromptPanel] = model.prompts
    prompts.foreach(panel => promptPanel.addPromptPanel(panel))
    if (!prompts.isEmpty) {
      promptPanel.setActivePrompt(0)
      okAction.enabled = false
    }
  }

  private val promptPanel = new JPromptPanelList()

  promptPanel.setRowHeight(50)

  initializePanel()
  contents = new MigPanel("debug,fill") {
    add(new Component {
      override lazy val peer = promptPanel
    }, "span 3,width 300, height 150, growx, growy, growprio 100, wrap")
    add(new Button(okAction), "width button!, growprio 0")
    add(new Button(cancelAction), "width button!, gapbefore 10")
  }

  def collectResult(): Option[Boolean] = Option(true)

}