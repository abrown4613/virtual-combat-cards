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

/**
 * A RadioButton ValuePanel.
 */
class RadioButtonValuePanel(label: String, options: List[String]) extends MigPanel("ins dialog, flowy") with ValuePanel[Int] {
  private val optionLabel = new Label(label + ":")
  private val buttons = options.map(x => new RadioButton(x))
  private val radios = new ButtonGroup(buttons: _*);
  private var selected = -1
  private val validRange = (0 until buttons.length)


  add(optionLabel)
  for (x <- buttons) {
    add(x)
    listenTo(x)
  }

  reactions += {
    case ButtonClicked(button) =>
      selected = buttons.indexOf(button)
      notifyListener()
  }

  def value(): Option[Int] = radios.selected.map(btn => options.indexOf(radios.selected.get.text))

  def setValue(value: Option[Int]) {
    if (value.isDefined && validRange.contains(value.get)) {
      radios.select(buttons(value.get))
    } else {
      radios.peer.clearSelection()
    }
  }
}
