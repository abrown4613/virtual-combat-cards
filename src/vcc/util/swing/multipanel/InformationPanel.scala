//$Id$
package vcc.util.swing.multipanel

import scala.swing._
import scala.swing.event._

class InformationPanel(wait:Boolean,message:String) extends MigPanel("") with AbstractPanel[Boolean] {
  private val messageLabel=new Label(message)
  private val okButton=new Button("Ok")
    
  add(messageLabel,"w 300,h 150,wrap")
  add(okButton,"align center")
    

  if(wait) {
    okButton.visible=true
    listenTo(okButton)
  } else {
    okButton.enabled=false
    okButton.visible=false
  }
  
  reactions += {
    case ButtonClicked(okButton) => 
       	remote ! 'OK
  }

  def returnHandler(msg:Any):Boolean = false.asInstanceOf[Boolean]
}
