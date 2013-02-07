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

abstract class TableModelRowProjection[T] {
  val columns: List[(String, java.lang.Class[_])]

  def size = columns.size

  val setter: PartialFunction[(Int, T, Any), Unit]

  def getColumnClass(col: Int): java.lang.Class[_] = columns(col)._2

  def getColumnName(col: Int): String = columns(col)._1

  def isEditable(col: Int, obj: T) = if (setter != null) setter.isDefinedAt(col, obj, null) else false

  def apply(col: Int, obj: T): java.lang.Object

  def set(col: Int, obj: T, value: Any) {
    if (setter != null && setter.isDefinedAt(col, obj, value)) setter(col, obj, value)
  }
}

class ProjectionTableModel[T](val proj: TableModelRowProjection[T]) extends javax.swing.table.AbstractTableModel {
  private var elem: Seq[T] = Nil

  def getValueAt(row: Int, col: Int): java.lang.Object = if (elem.size > row) proj(col, elem(row)) else null

  def getColumnCount: Int = proj.size

  def getRowCount: Int = elem.size

  override def getColumnName(col: Int) = proj.getColumnName(col)

  override def getColumnClass(col: Int) = proj.getColumnClass(col)

  override def isCellEditable(row: Int, col: Int) = (elem.size > row) && proj.isEditable(col, elem(row))

  override def setValueAt(value: java.lang.Object, row: Int, col: Int) {
    proj.set(col, elem(row), value)
  }

  def content_=(content: Seq[T]) {
    SwingHelper.invokeInEventDispatchThread {
      val s = elem.size
      elem = content
      if (s > content.size)
        this.fireTableRowsDeleted(content.size, s)
      this.fireTableDataChanged()
    }
  }

  def content = elem
}