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
package vcc.dnd4e.view

import swing._
import event._
import javax.swing.BorderFactory
import vcc.dnd4e.controller._
import util.swing.MigPanel

class CommentPanel(val controller:actors.Actor) extends MigPanel("fill","","") with ContextualView[ViewCombatant]{
  private var _hasChanged=false
  private var _updating=false
  private var edit=new TextArea {
    enabled=false
  }
  xLayoutAlignment=java.awt.Component.LEFT_ALIGNMENT;

  add(new Label("Comments and Notes"),"wrap")
  add(new ScrollPane {
    border=BorderFactory.createLoweredBevelBorder
    contents=edit
  },"growx,growy")
  
  listenTo(edit)
  reactions += {
    case FocusLost(edit:TextArea,opt,state) if(_hasChanged) =>
      sendChange()
    case ValueChanged(edit) =>
      if(!_updating) _hasChanged=true
  }
  
  private def sendChange() {
    if(_hasChanged) {
      _hasChanged=false
      controller ! actions.SetComment(context.id,edit.text)
    }
          
  }
  
  def changeContext(context:Option[ViewCombatant]) = {
    _updating=true
    if(_hasChanged) sendChange()
    edit.text=if(context.isDefined) context.get.info else ""
    edit.enabled=context!=None
    _updating=false
  }
}
