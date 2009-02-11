//$Id$
package vcc.dnd4e.view

import swing._
import event._
import javax.swing.BorderFactory
import vcc.dnd4e.controller._

class CommentPanel(val controller:actors.Actor) extends BoxPanel(Orientation.Vertical) with ContextualView[ViewCombatant]{
  private var _hasChanged=false
  private var _updating=false
  private var edit=new TextArea {
    enabled=false
  }
  xLayoutAlignment=java.awt.Component.LEFT_ALIGNMENT;

  border=BorderFactory.createTitledBorder("Comments and Notes")
  
  contents+= new ScrollPane {
    border=BorderFactory.createLoweredBevelBorder
    contents=edit
  }
  
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
