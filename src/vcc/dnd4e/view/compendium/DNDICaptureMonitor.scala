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
import vcc.app.dndi.CaptureHoldingArea
import vcc.domain.dndi.Monster
import vcc.domain.dndi.MonsterImportService

object DNDICaptureMonitor extends Frame {
  
  private val webserver = vcc.model.Registry.get[vcc.infra.webserver.WebServer]("webserver").get
  private val stateMessage = new Label()
  private var monsters:Seq[Monster] = Nil
  private val monsterList = new ListView[String](Nil)
  
  preferredSize = new java.awt.Dimension(300,400)
  iconImage = IconLibrary.MetalD20.getImage
  title = "D&D Insider Capture Monitor" 
  contents = new MigPanel("fill","[][][]","[][][]") {
    add(stateMessage,"wrap")
    add(new ScrollPane(monsterList), "span 3,growx, growy, wrap")
    add(new Button(Action("Import"){
      val sel = monsterList.selection.indices
      if(!sel.isEmpty) {
        for(idx <- sel) {
          MonsterImportService.importMonster(monsters(idx))
        }
      }
    }))
    add(new Button(Action("Close") { DNDICaptureMonitor.visible = false }))
  }
  
  private val startServerAction = Action("Start") { 
    webserver.start()
    toggleActionState()
  } 
  private val stopServerAction = Action("Stop") {
    webserver.stop()
    toggleActionState()
  }
  
  private def toggleActionState() {
    stopServerAction.enabled = webserver.running 
    startServerAction.enabled = ! webserver.running
    stateMessage.text = "Capture "+ (if(webserver.running) "Server is running" else "Server is stopped")
  }
  
  menuBar = { 
    val mb =new MenuBar()
    val serverMenu = new Menu("Server")
    serverMenu.contents += new MenuItem(startServerAction)
    serverMenu.contents += new MenuItem(stopServerAction)
    mb.contents += serverMenu
    val cacheMenu = new Menu("Cache")
    cacheMenu.contents += new MenuItem(Action("Clear Cache") { CaptureHoldingArea.clearCachedMonster() })
    cacheMenu.contents += new MenuItem(Action("Load cached entries"){ CaptureHoldingArea.loadCachedMonster()})
    mb.contents += cacheMenu
    mb
  }
  
  toggleActionState()
  CaptureHoldingArea.addMonsterObserver(new CaptureHoldingArea.CaptureHoldingObserver[Monster] {
     def updateContent(newContent: Seq[Monster]) {
       monsters = scala.util.Sorting.stableSort[Monster](newContent,(a:Monster,b:Monster)=> { a("NAME").get < b("NAME").get })
       monsterList.listData = monsters.map(monster => monster("NAME").get)
     }                                   
  })
  
}
