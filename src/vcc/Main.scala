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
package vcc


import vcc.infra.startup._
import scala.swing.Frame

object Main {

  def main(args : Array[String]) : Unit = {
    val frame = SplashWindow.showSplash(vcc.dnd4e.BootStrap)
    if(frame != null) {
      frame.visible = true
      println("Initialization complete.")
    } else {
      println("Initialization failed.")
    }
  }
}
