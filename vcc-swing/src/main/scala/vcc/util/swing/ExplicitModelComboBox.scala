/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.util.swing

import scala.swing._
import javax.swing._

/**
 * This is the base type for all typed ComboBoxModels
 */
trait TypedComboBoxModel[T] extends AbstractListModel with ComboBoxModel

/**
 * Model for ComboBox, it allows update to the contents via properties.
 * @param iv Initial value for the contents of the model
 */
class ContainerComboBoxModel[A](iv: Seq[A]) extends TypedComboBoxModel[A] {
  var entries: Seq[A] = iv

  private var selected: A = if (entries.isEmpty) null.asInstanceOf[A] else entries(0)

  def getSelectedItem: AnyRef = selected.asInstanceOf[AnyRef]

  def setSelectedItem(a: AnyRef) {
    selected = a.asInstanceOf[A]
  }

  def getElementAt(n: Int) = entries(n).asInstanceOf[AnyRef]

  def getSize = entries.length

  def contents_=(nv: Seq[A]) {
    entries = nv
    fireIntervalAdded(this, 0, nv.length)
  }

  def contents: Seq[A] = entries

}

/**
 * Renders a JList cell using a label that prints out what ever was returned by the format function.
 * @param format Returns the visual string for an object of type T
 */
class StringFormatListCellRenderer[T](format: T => String) extends JLabel with ListCellRenderer {
  def getListCellRendererComponent(model: JList, obj: AnyRef, index: Int, isSelected: Boolean, cellHasFocus: Boolean) = {
    if (obj != null) setText(format(obj.asInstanceOf[T]))
    this
  }
}

/**
 * This sub class of scala.swing.ComboBox is designed to be constructed based on a model
 * and not on a list. This allows model to be changed externally.
 */
class ExplicitModelComboBox[T](mdl: TypedComboBoxModel[T]) extends ComboBox[T](Nil) {
  override lazy val peer: JComboBox = new JComboBox(mdl) with SuperMixin

  /**
   * Defined the format renderer to be used with the ComboBox
   */
  def setFormatRenderer(fr: StringFormatListCellRenderer[T]) {
    peer.asInstanceOf[JComboBox].setRenderer(fr)
  }

}