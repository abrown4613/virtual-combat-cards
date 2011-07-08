/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.util.swing.dnd

import java.awt._
import datatransfer.DataFlavor
import dnd.{DragSourceDragEvent, DragSourceDropEvent, DragSourceMotionListener, DragSourceAdapter}
import javax.swing.UIManager

class DragAndDropGhost(icon: Image) extends DragSourceAdapter with DragSourceMotionListener {

  private val win = new DragWindow(icon)
  private var firstCall: Boolean = true


  def this() {
    this (null)
  }

  override def dragDropEnd(dsde: DragSourceDropEvent) {
    win.setVisible(false)
    firstCall = true
  }

  override def dragMouseMoved(dsde: DragSourceDragEvent) {
    val p: Point = dsde.getLocation
    p.translate(32, 16)
    win.setLocation(p)
    if (firstCall) {
      win.setVisible(true)
      val s = try {
        val tr = dsde.getDragSourceContext.getTransferable
        if (tr.isInstanceOf[GenTransferable[_]]) {
          tr.asInstanceOf[GenTransferable[_]].getDescription
        } else {
          val sf = dsde.getDragSourceContext.getTransferable.getTransferData(DataFlavor.stringFlavor).asInstanceOf[String]
          if (sf != null) sf else "..."
        }
      } catch {
        case _ => "..."
      }
      win.setLabel(s)
      firstCall = false
    }
  }

  private class DragWindow(icon: Image) extends Window(null) {

    this.setSize(30, 3 + icon.getHeight(null))
    private var message: String = ""
    setBackground(UIManager.getColor("control"))

    def setLabel(text: String) {
      message = text
      if (this.getGraphics != null) {
        val g: Graphics = this.getGraphics
        this.setSize(g.getFontMetrics.getStringBounds(message, g).getBounds.width + 30, this.getSize.height)
      }
    }

    override def paint(graphics: Graphics) {
      val g: Graphics2D = graphics.create.asInstanceOf[Graphics2D]
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.3f))
      if (icon != null) g.drawImage(icon, 1, 1, null)
      g.drawString(message, 25, 15)
      g.dispose()
    }
  }

}