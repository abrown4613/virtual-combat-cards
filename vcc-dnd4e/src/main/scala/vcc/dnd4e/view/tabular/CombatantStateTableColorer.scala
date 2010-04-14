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
package vcc.dnd4e.view.tabular

import vcc.util.swing.{ProjectionTableLabelFormatter}
import vcc.dnd4e.model.{CombatantState}

class CombatantStateTableColorer(var acting: Option[Symbol]) extends ProjectionTableLabelFormatter[CombatantState] {
  import java.awt.Color
  import vcc.dnd4e.model.common.InitiativeState._
  import vcc.dnd4e.model.common.HealthTracker.Status._

  private val fontSize = if (java.awt.Toolkit.getDefaultToolkit.getScreenSize().getHeight() > 7000) 14 else 12
  private val cellFont = new java.awt.Font(java.awt.Font.SANS_SERIF, java.awt.Font.PLAIN, fontSize)
  private val cellFontBold = new java.awt.Font(java.awt.Font.SANS_SERIF, java.awt.Font.BOLD, fontSize)

  // Pair[Color,Color]  where (background,foreground)
  private val grayed = (Color.LIGHT_GRAY, Color.BLACK)
  private val dead = (Color.BLACK, Color.WHITE)
  private val dying = (Color.GRAY, Color.WHITE)
  private val bloody = (new Color(220, 20, 60), Color.WHITE)
  private val ready = (Color.ORANGE, Color.BLACK)
  private val normal = (Color.WHITE, Color.BLACK)
  private val selected = (Color.BLUE, Color.WHITE)
  private val monsterCallout = (new Color(152, 32, 32), Color.WHITE)
  private val charCallout = (new Color(77, 140, 59), Color.WHITE)
  private val charBackground = (new Color(240, 255, 236), Color.BLACK)
  private val monsterBackground = normal //(new Color(255,248,220),Color.BLACK)

  private def setColor(label: javax.swing.JLabel, cp: Pair[Color, Color]): Unit = {
    label.setBackground(cp._1)
    label.setForeground(cp._2)
  }

  def render(label: javax.swing.JLabel, col: Int, isSelected: Boolean, cmb: CombatantState): Unit = {
    var is = cmb.init.state
    var hs = cmb.health.status
    val normalBack = if (cmb.isCharacter) charBackground else normal
    label.setFont(if (acting.isDefined && acting.get == cmb.id) cellFontBold else cellFont)
    label.setHorizontalAlignment(javax.swing.SwingConstants.CENTER)
    setColor(label, col match {
      case 0 => if (cmb.isCharacter) charCallout else monsterCallout
      case 3 =>
        hs match {
          case Dead => dead
          case Dying => dying
          case Bloody => bloody
          case _ if (isSelected) => (label.getBackground, label.getForeground)
          case _ if (is == Reserve) => grayed
          case _ => normalBack
        }
      case 5 if (is == Ready || is == Delaying) => ready
      case _ if (isSelected) => (label.getBackground, label.getForeground)
      case _ if (hs == Dead) => grayed
      case _ if (is == Reserve) => grayed
      case _ => normalBack
    })
  }

  def updateActing(nctx: Option[Symbol]) {
    acting = nctx
  }
}

