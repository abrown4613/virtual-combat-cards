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
