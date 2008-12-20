package vcc.util.swing

import scala.swing._ 
import scala.swing.event._
  
import javax.swing._
import javax.swing.table._
import javax.swing.event._
import java.awt.{Dimension, Color}
import scala.collection.mutable.Set

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