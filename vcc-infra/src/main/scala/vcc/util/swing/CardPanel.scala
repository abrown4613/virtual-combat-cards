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
//$Id
package vcc.util.swing

import java.awt.CardLayout
import javax.swing.JPanel
import swing.{Component, SequentialContainer, Panel}

/**
 * Creates a new card layout with the specified horizontal and
 * vertical gaps. The horizontal gaps are placed at the left and
 * right edges. The vertical gaps are placed at the top and bottom
 * edges.
 * @param hgap   the horizontal gap.
 * @param vgap   the vertical gap.
 */
class CardPanel(hgap: Int, vgap: Int) extends Panel with SequentialContainer.Wrapper {
  override lazy val peer = new JPanel(new CardLayout(hgap, vgap))
  private val cardLayout = peer.getLayout.asInstanceOf[CardLayout]
  /**
   * Create CardLayout backed pane with 0 gap
   */
  def this() = this (0, 0)

  /**
   * Add a component to a the CardLayout. The component must have a unique name.
   * @param component scala.swing.Component to be added to the panel, only the peer will be added.
   * @param key String identifier for the component in the CardLayout.
   */
  def addCard(component: Component, key: String) {
    peer.add(component.peer, key)
  }

  /**
   * Show a specific card, if card is not found, will not change the component. This is implemented using the using the
   * underlying CardLayout
   * @param key String identifier for the component in the CardLayout.
   */
  def showCard(key: String) {
    cardLayout.show(peer, key)
  }
}