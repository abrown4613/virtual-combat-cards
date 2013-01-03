/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
import vcc.util.swing.MigPanel
import vcc.dnd4e.view.IconLibrary
import vcc.infra.webserver.WebServer
import vcc.dndi.reader.DNDIObject
import vcc.dnd4e.compendium.MonsterImportService
import vcc.dndi.servlet.CaptureHoldingArea

object DNDICaptureMonitor {
  private var webServer:DNDICaptureMonitor = null

  def initialize(webServer: WebServer) {
    this.webServer = new DNDICaptureMonitor(webServer)
  }

  def getInstance() = webServer
}

class DNDICaptureMonitor(webServer: WebServer) extends Frame {

  private val thisWindow = this
  private val stateMessage = new Label()
  private var entries: Seq[DNDIObject] = Nil
  private val entryList = new ListView[String](Nil)

  preferredSize = new java.awt.Dimension(300, 400)
  iconImage = IconLibrary.MetalD20.getImage
  title = "D&D Insider Capture Monitor"
  contents = new MigPanel("ins dialog, fill", "[]", "[grow 0][fill][grow 0]") {
    add(stateMessage, "wrap")
    add(new ScrollPane(entryList), "grow, wrap")
    add(new Button(Action("Import") {
      val sel = entryList.selection.indices
      if (!sel.isEmpty) {
        for (idx <- sel) {
          MonsterImportService.importObject(entries(idx))
        }
      }
    }), "split 3")
    add(new Button(Action("Close") {
      thisWindow.visible = false
    }))
  }

  private val startServerAction = Action("Start") {
    webServer.start()
    toggleActionState()
  }
  private val stopServerAction = Action("Stop") {
    webServer.stop()
    toggleActionState()
  }

  private def toggleActionState() {
    stopServerAction.enabled = webServer.running
    startServerAction.enabled = !webServer.running
    stateMessage.text = "Capture " + (if (webServer.running) "Server is running" else "Server is stopped")
  }

  menuBar = {
    val mb = new MenuBar()
    val serverMenu = new Menu("Server")
    serverMenu.contents += new MenuItem(startServerAction)
    serverMenu.contents += new MenuItem(stopServerAction)
    mb.contents += serverMenu
    val cacheMenu = new Menu("Cache")
    cacheMenu.contents += new MenuItem(Action("Clear Cache") {
      val ret = Dialog.showConfirmation(stateMessage, "You are about to clear all monsters in your cache, are you sure?", "Clear captured monster cache", Dialog.Options.YesNoCancel)
      if (ret.id == 0) CaptureHoldingArea.getInstance.clearCachedMonster()
    })
    cacheMenu.contents += new MenuItem(Action("Load cached entries") {
      CaptureHoldingArea.getInstance.loadCachedEntries()
    })
    mb.contents += cacheMenu
    mb
  }

  toggleActionState()

  CaptureHoldingArea.getInstance.addObserver(new CaptureHoldingArea.CaptureHoldingObserver[DNDIObject] {
    def updateContent(newObject: DNDIObject, newContent: Seq[DNDIObject]) {
      entries = scala.util.Sorting.stableSort[DNDIObject](newContent, (a: DNDIObject, b: DNDIObject) => {a("base:name").get < b("base:name").get})
      entryList.listData = entries.map(monster => monster("base:name").get)
      if(newObject != null) {
        if(MonsterImportService.shouldImportAutomatically(newObject)) {
          MonsterImportService.importObject(newObject)
        }
      }
    }
  })
}