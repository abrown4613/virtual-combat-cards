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
import javax.swing.table._

object AlternativeTable {  
  private[swing] trait JTableMixin { def tableWrapper: AlternativeTable }
}


/**
 * Displays a matrix of items.
 * 
 * @see javax.swing.JTable
 */
class AlternativeTable extends Table with scala.swing.Scrollable with Publisher {

  override lazy val peer: JTable = new JTable with AlternativeTable.JTableMixin {
    def tableWrapper = AlternativeTable.this
    override def getCellRenderer(r: Int, c: Int) = new TableCellRenderer {
      def getTableCellRendererComponent(table: JTable, value: AnyRef, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int) = 
        AlternativeTable.this.rendererComponentFix(isSelected, hasFocus, row, column)
    }
    override def getCellEditor(r: Int, c: Int) = editor(r, c)
    override def getValueAt(r: Int, c: Int) = AlternativeTable.this.apply(r,c).asInstanceOf[AnyRef]
  }
  
  protected def rendererComponentFix(isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): java.awt.Component = { 
    val v = AlternativeTable.this.peer.getValueAt(row, column)
    if (v != null)
      AlternativeTable.this.peer.getDefaultRenderer(v.getClass).getTableCellRendererComponent(AlternativeTable.this.peer, 
                v, isSelected, hasFocus, row, column).asInstanceOf[JComponent]
    else AlternativeTable.this.peer.getDefaultRenderer(classOf[Object]).getTableCellRendererComponent(AlternativeTable.this.peer, 
                v, isSelected, hasFocus, row, column).asInstanceOf[JComponent]
  }
}