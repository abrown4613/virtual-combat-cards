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
//$Id$
package vcc.infra.prompter

import vcc.util.swing.MigPanel
import java.awt.Color
import java.awt.event.{ActionEvent, ActionListener}
import swing.{Button, Label, TextField}
import swing.event.{ButtonClicked, ValueChanged}

object TextFieldValuePanel {

  case class Return(value: Option[String]) extends ValuePanel.Return

}

/**
 * A TextField backed ValuePanel
 * @param question Question to place over the TextField. Should be generic and explain what should be placed in the
 * field.
 * @param validator A function that should accept a string and validate the output.
 */
class TextFieldValuePanel(question: String, validator: String => Boolean) extends MigPanel("ins dialog,  fill", "[]", "[][]push[]") with ValuePanel[String] {

  private val warning = new Color(255, 228, 196)
  protected val editField = new TextField
  editField.name = "editField"
  private val questionLabel = new Label(question)
  protected val acceptButton = new Button("Accept")
  acceptButton.name = "acceptButton"
  acceptButton.enabled = false

  add(questionLabel, "wrap")
  add(editField, "growx, wrap")
  add(acceptButton, "w button!")

  listenTo(editField, acceptButton)

  editField.peer.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      if (acceptButton.enabled) notifyListener(TextFieldValuePanel.Return(value))
    }
  })

  reactions += {
    case ValueChanged(`editField`) =>
      validateInput()
    case ButtonClicked(`acceptButton`) =>
      notifyListener(TextFieldValuePanel.Return(value))
  }

  //After setup, validate
  validateInput()

  private def validateInput() {
    val valid = validator(editField.text)
    acceptButton.enabled = valid
    editField.background = if (valid) Color.WHITE else warning
  }

  def value(): Option[String] = if (validator(editField.text)) Some(editField.text) else None

  def setValue(value: Option[String]) {
    if (value.isDefined) editField.text = value.get
    else editField.text = ""
  }

  def adjustFocus() {
    editField.requestFocus()
  }

  /**
   * Set the value of the input field
   * @param text Value to be placed
   */
  def setInputValue(text: String) {
    editField.text = text
  }
}