/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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

abstract class RowProjectionTable[T] extends EnhancedTable {
	protected var rowProjection:ProjectionTableModel[T]=null
 
	def projection_=(rp:ProjectionTableModel[T]) = {
	  rowProjection=rp
	  peer.setModel(rp)
	}
 
 	def projection = rowProjection
 
	def content = rowProjection.content
	def content_=(content:Seq[T]) = { rowProjection.content=content }
}

trait ProjectionTableLabelFormatter[T] {
  def render(label:javax.swing.JLabel,column:Int,isSelected:Boolean,entry:T)
}

trait CustomRenderedRowProjectionTable[T] extends RowProjectionTable[T] {
  
  val labelFormatter:ProjectionTableLabelFormatter[T]

  override def rendererComponentFix(isSelected: Boolean,hasFocus: Boolean, row: Int, column: Int): java.awt.Component= {
    var comp=super.rendererComponentFix(isSelected,hasFocus, row, column)
    if(comp.isInstanceOf[javax.swing.JLabel] && row<this.content.size) {
      labelFormatter.render(
        comp.asInstanceOf[javax.swing.JLabel],
        column,isSelected,
        this.content(row)
      )
    }
    comp
  }
}