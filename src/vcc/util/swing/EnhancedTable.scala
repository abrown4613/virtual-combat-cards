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
