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
package vcc.dnd4e.view.ruling

import vcc.infra.prompter.ValuePanel
import vcc.util.swing.MigPanel
import scala.swing._
import scala.swing.event._
import java.awt.event.{ActionEvent, ActionListener}
import vcc.dnd4e.domain.tracker.common.SaveEffectSpecialDecision

object SaveOrChangeValuePanel {

  case class Value(value: Option[SaveEffectSpecialDecision.Result]) extends ValuePanel.Return

  val Identity = "SaveSpecialPanel"
}

class SaveOrChangeValuePanel extends MigPanel("ins dialog", "[]", "[][][][]10:push[]") with ValuePanel[SaveEffectSpecialDecision.Result] {

  import SaveEffectSpecialDecision._
  import vcc.dnd4e.view.ruling.SaveOrChangeValuePanel.{Value}

  private val saveButton = new RadioButton("Save")
  private val changeButton = new RadioButton("Fail save and change effect to:")
  private val newConditionField = new TextField()
  private val acceptButton = new Button("Accept")
  private val buttonGroup = new ButtonGroup(saveButton, changeButton)
  // Be nice for testing
  newConditionField.name = "NewCondition"
  changeButton.name = "Change"
  acceptButton.name = "AcceptButton"

  add(new Label("Saving throw against effect?"), "wrap")
  add(saveButton, "wrap")
  add(changeButton, "wrap")
  add(newConditionField, "gap left 30, wrap, growx")
  add(acceptButton)

  setValue(None)

  listenTo(saveButton, changeButton, acceptButton)
  reactions += {
    case ButtonClicked(`saveButton`) =>
      acceptButton.enabled = true
      newConditionField.enabled = false
      notifyListener(Value(this.value))
    case ButtonClicked(`changeButton`) =>
      acceptButton.enabled = true
      newConditionField.enabled = true
      newConditionField.requestFocus()
    case ButtonClicked(`acceptButton`) =>
      notifyListener(Value(this.value))
  }

  newConditionField.peer.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent) {
      if (acceptButton.enabled) acceptButton.doClick()
    }
  })


  def adjustFocus() {
    saveButton.requestFocus()
  }

  def value: Option[SaveEffectSpecialDecision.Result] = {
    if (saveButton.selected) Some(Saved)
    else if (changeButton.selected) Some(Changed(newConditionField.text))
    else None
  }

  def setValue(value: Option[SaveEffectSpecialDecision.Result]) {
    value match {
      case None =>
        newConditionField.enabled = false
        acceptButton.enabled = false
        buttonGroup.peer.clearSelection()
      case Some(Saved) =>
        saveButton.selected = true
        acceptButton.enabled = true
      case Some(Changed(newCondition)) =>
        changeButton.selected = true
        acceptButton.enabled = true
        newConditionField.enabled = true
        setField("NewCondition", newCondition)
    }
  }

  override def setField(fieldName: String, value: String) {
    fieldName match {
      case "NewCondition" => newConditionField.text = value
      case _ => throw new IllegalArgumentException("Field '" + fieldName + "' unknown.")
    }
  }
}