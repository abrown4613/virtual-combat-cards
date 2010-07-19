/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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

import java.awt.{Window,Toolkit}
import scala.swing._
import javax.swing.JDialog

abstract class ModalDialog[T] (window:Window, title:String) extends UIElement with RootPanel with Publisher {
  
  //In order to work with the spash window we use low level AWT Window noa
  def this(owner: Frame,title:String) {
    this(owner.peer.getOwner,title)
  }
  override lazy val peer = new JDialog(window,title,java.awt.Dialog.ModalityType.APPLICATION_MODAL)
 
  protected var _result:Option[T] = None
  
  val okAction = Action("OK") { 
    processOK()
    visible = false
  }
  val cancelAction = Action("Cancel") { 
    visible = false 
  }
  
  def processOK()
  
  protected def dialogResult_=(v:Option[T]) { _result = v }
  
  def dialogResult:Option[T] = _result
  
  def placeOnScreenCenter() {
	val screen = Toolkit.getDefaultToolkit.getScreenSize
    val height = preferredSize.height + 35
    val width = preferredSize.width + 15
	peer.setBounds((screen.width - width)/2, (screen.height - height)/2,width,height)
  }
}
