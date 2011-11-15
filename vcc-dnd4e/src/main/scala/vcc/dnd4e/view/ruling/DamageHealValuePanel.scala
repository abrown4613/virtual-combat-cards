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

import net.miginfocom.swing.MigLayout
import vcc.infra.prompter.TextFieldValuePanel
import swing.{Action, Button}

object UnsignedIntegerMatcher {
  private val re = """^\s*(\d+)\s*$""".r
  val isUnsignedInt: String => Boolean = !re.findAllIn(_).isEmpty
  val asInt: String => Option[Int] = re.findFirstIn(_).map(_.trim.toInt)
}

class DamageHealValuePanel(question: String) extends TextFieldValuePanel(question, UnsignedIntegerMatcher.isUnsignedInt) {
  private val zeroButton = new Button(Action("Zero") {
    setValue(Some("0"))
    notifyListener(TextFieldValuePanel.Return(Some("0")))
  })
  insertButton()

  /**
   * This method is used to add a button to the parent panel by hacking the MigLayout and reformatting
   */
  private def insertButton() {
    val ml = peer.getLayout.asInstanceOf[MigLayout]
    val oldConst = ml.getComponentConstraints(acceptButton.peer)
    ml.setComponentConstraints(acceptButton.peer, oldConst + ", split 2")
    peer.add(zeroButton.peer, oldConst)
  }
}