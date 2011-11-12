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

import java.awt.{Desktop,Toolkit}
import javax.swing.SwingUtilities
import scala.swing.Frame

object SwingHelper {
  
  private val logger = org.slf4j.LoggerFactory.getLogger("infra")
  
  def makeRunnable(f: =>Unit):java.lang.Runnable =
    new java.lang.Runnable {
      def run() {
        f
      }
    } 
  
  def invokeLater(f: =>Unit) {
    javax.swing.SwingUtilities.invokeLater(makeRunnable(f))
  }
  
  def invokeInOtherThread( f: =>Unit) {
    if(SwingUtilities.isEventDispatchThread()) {
      var run=makeRunnable(f)
      val thr=new Thread(run)
      thr.start()
    } else {
      f
    }
  }
  
  /**
   * This function will invoke later if the current thread is not the
   * EventDispath thread, or immediately in the EventDispatchThread.
   * Use this avoid invokeLater inside and invokeLater
   * @param f The block to call
   */
  def invokeInEventDispatchThread(f: =>Unit) {
    if(SwingUtilities.isEventDispatchThread()) {
      f
    } else invokeLater(f)
  }
  
  def openDesktopBrowser(url:java.net.URL) {
    try {
      val dsk=java.awt.Desktop.getDesktop
      dsk.browse(url.toURI)
    }catch {
      case e => logger.error("Failed to open browser on URL: "+url,e)
    }
  }
  
  def centerFrameOnScreen(frame:Frame) {
	val screen = Toolkit.getDefaultToolkit.getScreenSize
    val height = frame.preferredSize.height
    val width = frame.preferredSize.width
	frame.peer.setBounds((screen.width - width)/2, (screen.height - height)/2,width,height)
  }
  
}

