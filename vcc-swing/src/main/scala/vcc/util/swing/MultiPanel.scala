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
package vcc.util.swing

import scala.swing._
import vcc.util.swing.multipanel._
import scala.actors.Actor.receive

trait MultiPanel extends RootPanel {

  private val controller = new Controller(this)
  private val selfActor = scala.actors.Actor.self
  controller.start()

  def showMessage(wait: Boolean, msg: String) {
    val p = new InformationPanel(wait, msg)
    controller ! SetPanel(selfActor, p)
    if (wait) receive {
      case s => p.returnHandler(s)
    }
  }

  /**
   * Set a generic panel
   * @param panel An AbstractPanel that will be used as a control
   * @return The value of what handler did
   */
  def customPanel[T](panel: AbstractPanel[T]): T = {
    controller ! SetPanel(selfActor, panel)
    receive {
      case s => panel.returnHandler(s)
    }
  }
}