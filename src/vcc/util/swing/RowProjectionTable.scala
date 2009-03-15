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