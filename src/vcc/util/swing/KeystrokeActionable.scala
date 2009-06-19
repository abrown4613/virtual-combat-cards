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

import scala.swing.{Component,Action}
import javax.swing.KeyStroke

object FocusCondition extends Enumeration {
    val WhenAncestorFocused = Value(javax.swing.JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
    val WhenFocused = Value(javax.swing.JComponent.WHEN_FOCUSED)
    val WhenWindowFocused = Value(javax.swing.JComponent.WHEN_IN_FOCUSED_WINDOW) 
}

/**
 * Provides KeyStroke binding to actions.
 */
trait KeystrokeActionable {
  self: Component =>
  
  /**
   * Associate a keystroke to an action
   * @param focus What is the focus condition to associated
   * @param keystroke String description of the key stroke to associated
   * @param action The action to be executed
   * @return True if the keystroke was associated 
   */
  def bindKeystrokeAction(focus: FocusCondition.Value,keystroke: String, action:Action):Boolean = {
    val ks=KeyStroke.getKeyStroke(keystroke)
    bindKeystrokeAction(focus,ks,action)
  }
  
  /**
   * Associate a keystroke to an action
   * @param keystroke String description of the key stroke to associated
   * @param action The action to be executed
   * @return True if the keystroke was associated 
   */
  def bindKeystrokeAction(keystroke: String, action:Action):Boolean = {
    bindKeystrokeAction(FocusCondition.WhenFocused, keystroke, action)
  }
  /**
   * Associate a keystroke to an action
   * @param focus What is the focus condition to associated
   * @param keystroke KeyStroke to associated
   * @param action The action to be executed
   * @return True if the keystroke was associated 
   */
  def bindKeystrokeAction(focus: FocusCondition.Value, keystroke: KeyStroke, action:Action):Boolean = {
    val im=self.peer.getInputMap(focus.id);
    if(im!=null && keystroke!=null) {
      im.put(keystroke,action.title)
      self.peer.getActionMap.put(action.title,action.peer)
      true
    } else {
      false
    }
  }
}
