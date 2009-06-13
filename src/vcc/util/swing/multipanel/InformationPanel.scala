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
