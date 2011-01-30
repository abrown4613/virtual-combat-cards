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
import swing._
import scala.swing.event.ButtonClicked

object EnumerationValuePanel {

  case class Value[E <: Enumeration](value: Option[E#Value]) extends ValuePanel.Return

}

/**
 * A RadioButton ValuePanel.
 */
class EnumerationValuePanel[E <: Enumeration](label: String, options: E) extends MigPanel("ins dialog, flowy") with ValuePanel[E#Value] {
  private val optionLabel = new Label(label + ":")
  private val buttons = options.values.map(x => new RadioButton(x.toString)).toSeq
  private val radios = new ButtonGroup(buttons: _*);
  private var selected: Option[E#Value] = None
  private val validRange = (0 until buttons.length)

  add(optionLabel)
  for (x <- buttons) {
    add(x)
    listenTo(x)
  }

  reactions += {
    case ButtonClicked(button) =>
      selected = enumFromButton(button)
      notifyListener(EnumerationValuePanel.Value(selected))
  }

  def value(): Option[E#Value] = radios.selected.map(btn => enumFromButton(btn).get)

  private def buttonFromEnum(e: E#Value): AbstractButton = buttons.find(b => b.text == e.toString).get

  private def enumFromButton(b: AbstractButton): Option[E#Value] = options.values.find(e => e.toString == b.text)

  def setValue(value: Option[E#Value]) {
    selected = value
    if (value.isDefined && value.get != null) {
      radios.select(buttonFromEnum(value.get))
    } else {
      radios.peer.clearSelection()
    }
  }

  def adjustFocus() {
    if (selected.isDefined) {
      buttonFromEnum(selected.get).requestFocus()
    } else {
      buttons(0).requestFocus
    }
  }
}