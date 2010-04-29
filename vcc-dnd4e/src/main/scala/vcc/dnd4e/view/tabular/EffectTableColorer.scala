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
package vcc.dnd4e.view.tabular

import vcc.util.swing.ProjectionTableLabelFormatter
import vcc.dnd4e.domain.tracker.common.Effect

object EffectTableColorer extends ProjectionTableLabelFormatter[Effect] {
  import java.awt.Color

  private val attention = (Color.ORANGE, Color.BLACK)
  private val beneficial = (new Color(152, 251, 152), Color.BLACK)
  private val normal = (Color.WHITE, Color.BLACK)

  def setColor(label: javax.swing.JLabel, colors: (Color, Color)) {
    label.setBackground(colors._1)
    label.setForeground(colors._2)
  }

  def render(label: javax.swing.JLabel, col: Int, isSelected: Boolean, effect: Effect) {
    val needAttention = effect.sustainable || effect.duration == Effect.Duration.SaveEndSpecial

    setColor(label, col match {
      case _ if (isSelected) => (label.getBackground, label.getForeground)
      case 1 if (needAttention) => attention
      case _ if (effect.condition.beneficial) => beneficial
      case _ => normal
    })
  }
}
