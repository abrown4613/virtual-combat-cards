/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.util.swing

import javax.swing.JFormattedTextField
import scala.swing.{TextComponent, FormattedTextField}

/**
 * A text field with formatted input.
 *
 * @see javax.swing.JFormattedTextField
 */
class FormattedTextFieldWithFormatter(format: JFormattedTextField.AbstractFormatter) extends TextComponent {
  override lazy val peer: JFormattedTextField = new JFormattedTextField(format)

  import FormattedTextField._

  def commitEdit() {peer.commitEdit()}

  def editValid: Boolean = peer.isEditValid

  def focusLostBehavior: FocusLostBehavior.Value = FocusLostBehavior(peer.getFocusLostBehavior)

  def focusLostBehavior_=(b: FocusLostBehavior.Value) {peer.setFocusLostBehavior(b.id)}
}