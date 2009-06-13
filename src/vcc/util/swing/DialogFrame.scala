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
package vcc.util.swing

import scala.swing._
import scala.swing.event._

case class DialogClosed(dialog:DialogFrame,cancel:Boolean) extends scala.swing.event.Event

abstract class DialogFrame extends Frame {
  val okButton=new Button("Ok")
  val cancelButton=new Button("Cancel")
  
  var mainpanel=new BorderPanel {
    var userarea:Component=null
    def setContent(comp:Component) = {userarea=comp; this.add(comp,BorderPanel.Position.Center)}
    def getContent():Component = userarea;
    
    add(new FlowPanel{
          contents+=okButton
          contents+=cancelButton
        },BorderPanel.Position.South)
  }
  listenTo(okButton,cancelButton)
  super.contents=mainpanel
  reactions +={
    case ButtonClicked(this.okButton)=>  publish(DialogClosed(this,false)); visible=false
    case ButtonClicked(this.cancelButton)=> publish(DialogClosed(this,true)); visible=false
  }
  
  override def contents_=(comp:Component):Unit=mainpanel.setContent(comp)
  override def contents=Seq(mainpanel.getContent())
}
