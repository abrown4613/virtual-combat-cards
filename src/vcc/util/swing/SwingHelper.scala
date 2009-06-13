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

object SwingHelper {
  import javax.swing.SwingUtilities
  
  def makeRunnable(f:() =>Unit):java.lang.Runnable =
    new java.lang.Runnable {
      def run() {
        f.apply()
      }
    } 
  
  def invokeLater(f:()=>Unit) {
    javax.swing.SwingUtilities.invokeLater(makeRunnable(f))
  }
  
  def invokeInOtherThread( f: () =>Unit) {
    if(SwingUtilities.isEventDispatchThread()) {
      var run=makeRunnable(f)
      val thr=new Thread(run)
      thr.start()
    } else {
      f
    }
  }
  
  def openDesktopBrowser(url:java.net.URL) {
    try {
      val dsk=java.awt.Desktop.getDesktop
      dsk.browse(url.toURI)
    }catch {
      case _ => println("Failed to open browser on URL: "+url)
    }
  }
}

