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
import javax.swing.JDialog

abstract class ModalDialog[T] (owner: Frame, title:String) extends UIElement with RootPanel with Publisher
{
  override lazy val peer = new JDialog(owner.peer,title,true)
 
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
}
