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
import swing.{Label, TextField}
import swing.event.ValueChanged
import java.awt.Color

class TextFieldValuePanel(question: String, validator: String => Boolean) extends MigPanel("ins dialog, fill") with ValuePanel[String] {

  private val warning = new Color(255, 228, 196)
  private val editField = new TextField
  editField.name = "editField"
  private val questionLabel = new Label(question)

  add(questionLabel, "wrap")
  add(editField, "growx, wrap")

  listenTo(editField)

  reactions += {
    case ValueChanged(`editField`) =>
      validateInput()
  }

  //After setup, validate
  validateInput()

  private def validateInput() {
    val valid = validator(editField.text)
    if (mediator != null) {
      val v = if (valid) Some(editField.text) else None
      mediator.valuePanelChanged(v)
    }
    editField.background = if (valid) Color.WHITE else warning
  }

  def value(): Option[String] = if (validator(editField.text)) Some(editField.text) else None

  def setValue(value: Option[String]) {
    if (value.isDefined) editField.text = value.get
    else editField.text = ""
  }
}