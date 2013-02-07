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

import javax.swing.JComponent
import scala.swing.Component

case class DockableComponentWrapper(dockID:DockID,dockTitle:String,dockRootComponent:JComponent,dockFocusComponent:JComponent) extends DockableComponent 

object DockableComponentWrapper {
  def apply(id:DockID,title:String,component:Component,primaryFocus:Component):DockableComponentWrapper = apply(id,title,component.peer,if(primaryFocus!=null)primaryFocus.peer else null)
}

trait DockableComponent {
  def dockTitle:String
  def dockID:DockID
  def dockRootComponent:JComponent
  def dockFocusComponent:JComponent
}

trait ScalaDockableComponent extends DockableComponent {
  self : scala.swing.Component =>
  
  def dockRootComponent:JComponent = self.peer
}


class DockableNotDefined(key:DockID) extends Exception("Cant find Dockable with ID: "+key.name)

trait DockingFrameworkAdapter[D] extends DockingActionController {

  //Utitity Dock Map
  class DockableMap {
    private var _map = Map.empty[DockID,(DockableComponent,D)]
    
    def add(ddef:DockableComponent) {
      _map = _map + (ddef.dockID -> (ddef,createFrameworkDockable(ddef)))
    }
    def remove(key:DockID) {
      _map = _map - key
    }
    
    def dockableComponent(key:DockID):DockableComponent = {
      if(!_map.isDefinedAt(key)) throw new DockableNotDefined(key)
      _map(key)._1
    }
    
    def dockable(key:DockID):D = {
      if(!_map.isDefinedAt(key)) throw new DockableNotDefined(key)
      _map(key)._2
    }
    
    def keys = _map.keys
  }
  protected val dockMap = new DockableMap()
  
  def addDockable(ddef:DockableComponent) {
    dockMap.add(ddef)
    registerDockable(ddef,dockMap.dockable(ddef.dockID))
  }

  protected def createFrameworkDockable(ddef:DockableComponent):D
  
  protected def registerDockable(ddef:DockableComponent,dock:D) 
  
  def setup(owner:java.awt.Window):JComponent
  
  def restoreDefaultLayout()
}
