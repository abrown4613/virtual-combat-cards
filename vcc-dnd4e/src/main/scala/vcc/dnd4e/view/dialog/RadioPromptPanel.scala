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

import org.exnebula.swing.PromptPanel
import vcc.dnd4e.view.dialog.RadioPromptPanel.Choice
import java.lang.String
import javax.swing.JComponent
import org.exnebula.swing.PromptPanel.EditCompletionListener
import vcc.util.swing.MigPanel
import swing.event.ButtonClicked
import swing.{Label, ButtonGroup, RadioButton}

object RadioPromptPanel {
  case class Choice[T](name: String, value:T)
}

class RadioPromptPanel[T](val title:String,  val choices:Choice[T]*) extends PromptPanel {

  private var editCompletionListener: EditCompletionListener = null
  private val buttons = choices.map(c => makeRadioButton(c))
  private val radioGroup = new ButtonGroup(buttons: _*)
  private val statusLabel = new Label("Click to change")
  private val editorPanel = createEditorPanel()
  private val viewPanel = createViewPanel()

  def setEditCompletionListener(listener: EditCompletionListener) {
    editCompletionListener = listener
  }

  def getViewComponent: JComponent = viewPanel.peer

  def getEditorComponent: JComponent = editorPanel.peer

  def adjustFocusToEditor() {
    radioGroup.selected.getOrElse(buttons(0)).requestFocus()
  }

  def hasValidAnswer = radioGroup.selected.isDefined

  def getPromptTile: String = title

  private def makeRadioButton(value: Choice[T]): scala.swing.RadioButton = {
    new RadioButton(value.name)
  }

  private def createEditorPanel() = {
    new MigPanel("") {
      buttons.foreach(b => add(b, "gap 20"))
      listenTo(buttons: _*)
      reactions += {
        case ButtonClicked(button) =>
          editCompletionListener.editComplete()
          statusLabel.text = button.text
      }
    }
  }

  private def createViewPanel() = {
    new MigPanel("fill") {
      add(statusLabel, "alignx right")
    }
  }
}