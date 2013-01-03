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

import scala.swing._
import net.miginfocom.swing._
import javax.swing.JPanel

/**
 * MigPanel controlled layout panel. See MigPanel documentation on options for this panel.
 * @param layoutConstrains General template layout
 * @param colConstraints Columns constraints
 * @param rowConstraints Row constraints
 */
class MigPanel(layoutConstrains: String, colConstraints: String, rowConstraints: String) extends Panel with SequentialContainer.Wrapper {
  override lazy val peer = new JPanel(new MigLayout(layoutConstrains, colConstraints, rowConstraints))

  def this(layoutConstraints: String) = this (layoutConstraints, "", "")

  private def layoutManager = peer.getLayout.asInstanceOf[MigLayout]

  /**
   * Add a component with MigLayout base layout options.
   * @param c Component to add
   * @param layout The MigLayout specific format string.
   */
  protected def add(c: Component, layout: String) {
    peer.add(c.peer, layout)
  }

  /**
   * Add a component to the panel.
   * @param c Component to add
   */
  protected def add(c: Component) {
    peer.add(c.peer)
  }

  /**
   * Introduce a separator with a possible title.
   * @param title May be null or blank to indicate you just want a line.
   */
  protected def addSeparator(title: String) {
    if (title != null && !"".equals(title))
      add(new Label(title), "gapbottom 1, span, split 2");
    add(new Component {
      override lazy val peer = new javax.swing.JSeparator()
    }, "gapleft rel, growx,wrap");
  }

  /**
   * Get minimum layout size for the internal content
   */
  def minimumLayoutSize = layoutManager.minimumLayoutSize(this.peer)

  /**
   * Get preferred layout size for the internal content
   */
  def preferredLayoutSize = layoutManager.preferredLayoutSize(this.peer)
}
