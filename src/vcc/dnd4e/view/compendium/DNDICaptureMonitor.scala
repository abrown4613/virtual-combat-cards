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
package vcc.dnd4e.view.compendium

import scala.swing._
import scala.swing.event._
import vcc.util.swing.MigPanel

object DNDICaptureMonitor extends Frame {
  preferredSize = new java.awt.Dimension(300,400)
  contents = new MigPanel("fill","[][][]","[][][]") {
    add(new Label("Server Status: stoppped"),"wrap")
    add(new ScrollPane(new ListView(List("ab","cd"))), "span 3,growx, growy, wrap")
    add(new Button("Import"))
    add(new Button(Action("Close") { DNDICaptureMonitor.visible = false }))
  }

  var serverActive = false 
  private val startServerAction = Action("Start") { 
    serverActive = true
    toggleActionState()
  } 
  private val stopServerAction = Action("Stop") {
    serverActive = false
    toggleActionState()
  }
  
  private def toggleActionState() {
    stopServerAction.enabled = serverActive 
    startServerAction.enabled = ! serverActive
  }
  
  menuBar = { 
    val mb =new MenuBar()
    val serverMenu = new Menu("Server")
    serverMenu.contents += new MenuItem(startServerAction)
    serverMenu.contents += new MenuItem(stopServerAction)
    mb.contents += serverMenu
    val cacheMenu = new Menu("Server")
    cacheMenu.contents += new MenuItem(Action("Clear Cache") { })
    cacheMenu.contents += new MenuItem(Action("Load cached entries"){})
    mb.contents += cacheMenu
    mb
  }
  
  toggleActionState()
}
