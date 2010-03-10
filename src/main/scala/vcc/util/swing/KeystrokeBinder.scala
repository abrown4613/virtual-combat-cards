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

import scala.swing.{Component,Action,Button}
import javax.swing.KeyStroke

/**
 * Utility class to wrap a button click as an action. Use this to bind 
 * keys to button at high level without the need to create a anonymous class
 * @param name The name of the action
 * @param btn The button to be clicked as part of this action
 */
class ClickButtonAction(name:String, btn:Button) extends Action(name) {
  def apply() {
    btn.doClick
  }
}

/**
 * Utility trait to allow component that need to register actions
 * after they are in part of the RootPane to be identified.
 */
trait KeystrokeContainer {
  def registerKeystroke() 
}

/**
 * Provides KeyStroke binding to actions.
 */
object KeystrokeBinder {
  
  object FocusCondition extends Enumeration {
    val WhenAncestorFocused = Value(javax.swing.JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
    val WhenFocused = Value(javax.swing.JComponent.WHEN_FOCUSED)
    val WhenWindowFocused = Value(javax.swing.JComponent.WHEN_IN_FOCUSED_WINDOW) 
  }

  /**
   * Associate a keystroke to an action
   * @param focus What is the focus condition to associated
   * @param keystroke String description of the key stroke to associated
   * @param action The action to be executed
   * @return True if the keystroke was associated 
   */
  def bindKeystrokeAction(component:Component, bindToRootPane:Boolean, focus: FocusCondition.Value,keystroke: String, action:Action):Boolean = {
    val ks=KeyStroke.getKeyStroke(keystroke)
    bindKeystrokeAction(component,bindToRootPane,focus,ks,action)
  }
  
  /**
   * Associate a keystroke to an action, but only when focused
   * @param keystroke String description of the key stroke to associated
   * @param action The action to be executed
   * @return True if the keystroke was associated 
   */
  def bindKeystrokeAction(component:Component, bindToRootPane:Boolean, keystroke: String, action:Action):Boolean = {
    bindKeystrokeAction(component,bindToRootPane,FocusCondition.WhenFocused, keystroke, action)
  }
  /**
   * Associate a keystroke to an action
   * @param focus What is the focus condition to associated
   * @param keystroke KeyStroke to associated
   * @param action The action to be executed
   * @return True if the keystroke was associated 
   */
  def bindKeystrokeAction(component:Component, bindToRootPane:Boolean, focus: FocusCondition.Value, keystroke: KeyStroke, action:Action):Boolean = {
    val tgt = if(bindToRootPane) component.peer.getRootPane else component.peer
    val im=tgt.getInputMap(focus.id)
    if(im!=null && keystroke!=null) {
      im.put(keystroke,action.title)
      tgt.getActionMap.put(action.title,action.peer)
      true
    } else {
      false
    }
  }
  
  def unbindKeystroke(component:Component, boundToRootPane:Boolean,focus: FocusCondition.Value, keystroke:String):Boolean = {
    val ks = KeyStroke.getKeyStroke(keystroke)
    unbindKeystroke(component,boundToRootPane,focus,ks)
  }
  def unbindKeystroke(component:Component, boundToRootPane:Boolean,focus: FocusCondition.Value, keystroke:KeyStroke):Boolean = {
    val tgt = if(boundToRootPane) component.peer.getRootPane else component.peer
    val im=tgt.getInputMap(focus.id)
    if(im!=null && keystroke!=null) {
      im. put(keystroke,"none")
      true
    } else {
      false
    }
    
  }
}
