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
package vcc.dnd4e.view.ruling

import vcc.infra.prompter.ValuePanel
import vcc.util.swing.MigPanel
import scala.swing._
import scala.swing.event._
import vcc.dnd4e.view.ruling.SaveOrChangeValuePanel.SaveResult
import java.awt.event.{ActionEvent, ActionListener}

object SaveOrChangeValuePanel {

  case class Value(value: Option[SaveResult]) extends ValuePanel.Return

  sealed trait SaveResult

  case object Saved extends SaveResult

  case class Changed(to: String) extends SaveResult

  val Identity = "SaveSpecialPanel"
}

class SaveOrChangeValuePanel extends MigPanel("ins dialog", "[]", "[][][][]10:push[]") with ValuePanel[SaveResult] {

  import SaveOrChangeValuePanel._

  private val saveButton = new RadioButton("Save")
  private val changeButton = new RadioButton("Failed and changed effect to:")
  private val newConditionField = new TextField()
  private val acceptButton = new Button("Accept")
  private val buttonGroup = new ButtonGroup(saveButton, changeButton)
  // Be nice for testing
  newConditionField.name = "NewCondition"
  changeButton.name = "Change"
  acceptButton.name = "AcceptButton"

  add(new Label("Save against effect?"), "wrap")
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
      notifyListener(Value(Some(Saved)))
    case ButtonClicked(`changeButton`) =>
      acceptButton.enabled = true
      newConditionField.enabled = true
    case ButtonClicked(`acceptButton`) =>
      if (saveButton.selected) notifyListener(Value(Some(Saved)))
      else notifyListener(Value(Some(Changed(newConditionField.text))))
  }

  newConditionField.peer.addActionListener(new ActionListener() {
    def actionPerformed(e: ActionEvent): Unit = {
      if (acceptButton.enabled) acceptButton.doClick
    }
  })


  def adjustFocus() {

  }

  def value(): Option[SaveResult] = None

  def setValue(value: Option[SaveResult]) {
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
        setNewCondition(newCondition)
    }
  }

  def setNewCondition(condition: String) {
    newConditionField.text = condition
  }
}