//$Id$
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
package vcc.infra.docking.idw

import net.infonode.docking._
import javax.swing.JComponent
import java.io.{OutputStream,InputStream}

abstract class InfoNodeDockAdapter extends DockingFrameworkAdapter[View]   {
  
  protected val vm = new util.StringViewMap()
  protected var root:RootWindow = null

  protected def createFrameworkDockable(ddef:DockableComponent):View = {
    new View (ddef.dockTitle, null, ddef.dockRootComponent)
  }
  
  protected def registerDockable(ddef:DockableComponent,dockable:View) {
    vm.addView(ddef.dockID.name,dockable)
  }
  
  def defaultSetup(owner:java.awt.Window):JComponent = {
    root = util.DockingUtil.createRootWindow(vm,true)
    root
  }
  
  def restoreFocus(id:DockID) {
    val view = dockMap.dockable(id)
    val dock = dockMap.dockableComponent(id)
    view.restoreFocus()
    if(dock.dockFocusComponent != null)
    vcc.util.swing.SwingHelper.invokeLater {
      dock.dockFocusComponent.requestFocus()
    }
  }

  def restore(id:DockID) {
    val dockable = dockMap.dockable(id)
    dockable.restore()
  }

  def storeLayout(out:OutputStream) {
	val oos = new java.io.ObjectOutputStream(out);
	root.write(oos);
	oos.close();
  }

  def restoreLayout(in:InputStream) {
	val ois = new java.io.ObjectInputStream(in);
	root.read(ois);
	ois.close();
    
  }
}
