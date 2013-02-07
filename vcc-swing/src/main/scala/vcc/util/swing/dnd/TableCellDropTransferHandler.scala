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
package vcc.util.swing.dnd

import swing.Table
import javax.swing.TransferHandler.TransferSupport
import javax.swing.{DropMode, JTable, TransferHandler}
import java.awt.datatransfer.DataFlavor

/**
 * CellDrop contains data relative to the drop.
 * @param row Cell row
 * @param column Cell column
 * @param data Data being dropped
 */
sealed case class CellDrop(row: Int, column: Int, data: AnyRef)

/**
 * This class adds behavior to tabled to enable drop on a table. It allows users to register listeners for a give
 * DataFlavor.
 */
class TableCellDropTransferHandler() extends TransferHandler() {

  var handlers = List.empty[(DataFlavor, PartialFunction[CellDrop, Boolean])]

  /**
   * Activate Cell drop on a give table.
   * @param table Table to activate drag on.
   */
  def decorateTable(table: Table) {
    table.peer.setDragEnabled(true)
    table.peer.setDropMode(DropMode.ON)
    table.peer.setTransferHandler(this)
  }

  private def getCellLocation(support: TransferSupport): (Int, Int) = {
    val dl = support.getDropLocation.asInstanceOf[JTable.DropLocation]
    (dl.getRow, dl.getColumn)
  }

  /**
   * Register interest on a given DataFlavor and associate a handler to receive it.
   * @param what DataFlavor to look for on the Transferable
   * @param handler Partial function that handled (row, column, Transferable), it must be defined if it will receive the Transferable
   *                and return true if import was successful.
   */
  def interestedIn(what: DataFlavor)(handler: PartialFunction[CellDrop, Boolean]) {
    handlers = handlers ::: List((what, handler))
  }

  private def findHandler(row: Int, col: Int, support: TransferSupport) = {
    handlers.find(
      hp => support.isDataFlavorSupported(hp._1) &&
        hp._2.isDefinedAt(CellDrop(row, col, support.getTransferable.getTransferData(hp._1))))
  }

  override def importData(support: TransferSupport): Boolean = {
    val (row, col) = getCellLocation(support)
    //If canImport we have a handler
    if (canImport(support)) {
      try {
        val r = findHandler(row, col, support)
        val (flavor, handler) = r.get
        handler(CellDrop(row, col, support.getTransferable.getTransferData(flavor)))
      }
      catch {
        case e: Exception =>
          false
      }
    } else {
      false
    }
  }

  override def canImport(support: TransferSupport): Boolean = {
    val (row, col) = getCellLocation(support)
    if (support.isDrop && (row != -1 && col != -1)) {
      findHandler(row, col, support).isDefined
    } else false
  }
}