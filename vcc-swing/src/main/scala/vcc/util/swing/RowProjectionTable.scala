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

import java.awt.Color
import javax.swing.{JLabel, JComponent}

abstract class RowProjectionTable[T] extends EnhancedTable {
  protected var rowProjection: ProjectionTableModel[T] = null

  def projection_=(rp: ProjectionTableModel[T]) {
    rowProjection = rp
    peer.setModel(rp)
  }

  def projection = rowProjection

  def content = rowProjection.content

  def content_=(content: Seq[T]) {
    rowProjection.content = content
  }
}

trait ProjectionTableLabelFormatter[T] {

  /**
   * Helper method to set the component's color
   * @param component Component to be colored
   * @param cp A pair of colors meaning (Background,Foreground)
   */
  protected def setColorPair(component: JComponent, cp: Pair[Color, Color]) {
    component.setBackground(cp._1)
    component.setForeground(cp._2)
  }

  /**
   * Helper method to get the component's color
   * @return A pair of colors meaning (Background,Foreground)
   */
  protected def getColorPair(component: JComponent): Pair[Color, Color] = (component.getBackground, component.getForeground)

  def render(label: javax.swing.JLabel, column: Int, isSelected: Boolean, isDropTarget: Boolean, entry: T)
}

trait CustomRenderedRowProjectionTable[T] extends RowProjectionTable[T] {
  val labelFormatter: ProjectionTableLabelFormatter[T]

  @inline protected def isDropLocation(row: Int, column: Int): Boolean = {
    val dropLocation = this.peer.getDropLocation
    (dropLocation != null
      && !dropLocation.isInsertRow && !dropLocation.isInsertColumn
      && dropLocation.getRow == row && dropLocation.getColumn == column)
  }

  override def rendererComponentFix(isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): java.awt.Component = {

    val comp = super.rendererComponentFix(isSelected, hasFocus, row, column)
    if (comp.isInstanceOf[JLabel] && row < this.content.size) {
      labelFormatter.render(
        comp.asInstanceOf[javax.swing.JLabel],
        column, isSelected, isDropLocation(row, column),
        this.content(row)
      )
    }
    comp
  }
}