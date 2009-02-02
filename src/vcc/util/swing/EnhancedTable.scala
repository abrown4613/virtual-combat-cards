//$Id$
package vcc.util.swing

import scala.swing._
import scala.swing.event._

class EnhancedTable extends AlternativeTable {
  
  peer.getTableHeader().setReorderingAllowed(false)

  def setColumnWidth(col:Int,width:Int):Unit = this.setColumnWidth(col,width,width/2,width*2)
  
  def setColumnWidth(col:Int,preferred:Int,min:Int,max:Int):Unit = {
    if(col<peer.getColumnCount) {
      var column=peer.getColumnModel.getColumn(col)
      column.setPreferredWidth(preferred)
      column.setMinWidth(min)
      column.setMaxWidth(max)
    }
  }
}
