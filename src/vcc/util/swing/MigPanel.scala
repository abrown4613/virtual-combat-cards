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
import net.miginfocom.swing._

class MigPanel(layoutConstrains:String, colConstriants:String, rowConstraints:String) extends Panel with SequentialContainer.Wrapper {
  override lazy val peer = new javax.swing.JPanel(new MigLayout(layoutConstrains,colConstriants,rowConstraints))

  def this(layoutContraints:String) = this(layoutContraints,"","")
  
  private def layoutManager = peer.getLayout.asInstanceOf[MigLayout]
  
  protected def add(c: Component, l: String) { peer.add(c.peer, l) }
  
  protected def add(c: Component) { peer.add(c.peer)}
  
  protected def addSeparator(title:String) {
    if (title != null && !"".equals(title))
      add(new Label(title), "gapbottom 1, span, split 2");
    add(new Component { override lazy val peer= new javax.swing.JSeparator()}, "gapleft rel, growx,wrap");
  }

  
}
