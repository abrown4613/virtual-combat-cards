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
package vcc.dnd4e.view

import javax.swing.ImageIcon
import vcc.util.swing.AbortApplication

/**
 * This is a collection of images loaded for the view components.
 */
object IconLibrary {
  protected def loadIcon(resource: String): ImageIcon = {
    val url = this.getClass.getResource(resource)
    if (url != null) {
      val icon = new ImageIcon(url)
      icon
    } else {
      AbortApplication(this, "Failed to load resource: " + resource, null)
    }
  }

  val MetalD20 = loadIcon("/vcc/dnd4e/images/d20metal.png")

  val ActionGO = loadIcon("/vcc/dnd4e/images/action_go.gif")

  val MicroArrowGrey = loadIcon("/vcc/dnd4e/images/mi_arrow_grey.png")

  val MicroBlank = loadIcon("/vcc/dnd4e/images/mi_blank.png")

  val MiniWand = loadIcon("/vcc/dnd4e/images/icon_wand.gif")
}
