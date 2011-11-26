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
import swing.{Label, ButtonGroup, RadioButton}

object RadioPromptPanel {
  case class Choice[T](name: String, value:T)
}

class RadioPromptPanel[O](val title:String,  val choices:Choice[O]*) extends PromptPanel {

  private val editorPanel = new MigPanel("debug") {

    def makeRadioButton(value: Choice[O]): scala.swing.RadioButton = {
      val radio = new RadioButton(value.name)
      radio
    }

    val buttons = choices.map(c => makeRadioButton(c))
    private val radioGroup = new ButtonGroup(buttons: _*)
    buttons.foreach(b => add(b, "gap 20"))
  }

  def setEditCompletionListener(listener: EditCompletionListener) {}

  def getViewComponent: JComponent = (new Label("Not here")).peer

  def getEditorComponent: JComponent = editorPanel.peer

  def adjustFocusToEditor() {}

  def getPromptTile: String = title
}