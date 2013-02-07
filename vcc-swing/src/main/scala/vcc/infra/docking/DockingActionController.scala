/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.infra.docking

import java.io.{OutputStream,InputStream}

case class DockID(name:String)

trait DockingActionController {
  
  def restore(dock:DockID)
  
  def restoreFocus(dock:DockID)
  
  def storeLayout(os:OutputStream)
  
  def restoreLayout(in:InputStream)
  
  def restoreDefaultLayout()
}

import scala.swing.Action

class DockableRestoreAction(docker:DockingActionController,id:DockID,text:String) extends Action(text) {
  def apply() {
	docker.restore(id)
  }
}

class DockableFocusAction(docker:DockingActionController,id:DockID,text:String) extends Action(text) {
  def apply() {
	docker.restoreFocus(id)
  }
}


