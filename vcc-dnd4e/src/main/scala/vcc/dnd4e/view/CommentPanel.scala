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
package vcc.dnd4e.view

import swing._
import event._
import javax.swing.BorderFactory
import vcc.util.swing.MigPanel
import vcc.infra.docking.{DockableComponent}

abstract class CommentPanel extends MigPanel("fill,ins 0", "", "") with DockableComponent {

  // User has changed the text
  private var _hasChanged = false

  //An non-user update is happening (see updateText)
  private var _updating = false
  private val edit = new TextArea {
    enabled = false
  }

  xLayoutAlignment = java.awt.Component.LEFT_ALIGNMENT;

  val dockRootComponent = this.peer

  val dockFocusComponent = edit.peer

  add(new ScrollPane {
    border = BorderFactory.createLoweredBevelBorder
    contents = edit
  }, "growx,growy")

  listenTo(edit)

  reactions += {
    case FocusLost(edit: TextArea, opt, temp) if (_hasChanged) => sendChange()
    case ValueChanged(edit) if (!_updating) => _hasChanged = true
  }

  def sendChangeMessage(text: String)

  def hasChanged = _hasChanged

  def editorEnabled_=(enable: Boolean) {edit.enabled = enable}

  def editorEnabled = edit.enabled

  /**
   * Send a change if their is some change has happened.
   */
  protected def sendChange() {
    if (_hasChanged) {
      _hasChanged = false
      sendChangeMessage(edit.text)
    }
  }

  /**
   * Update text externally without indicating that a user driven change has happened
   */
  def updateText(text: String) {
    _updating = true
    edit.text = text
    _updating = false

  }

}