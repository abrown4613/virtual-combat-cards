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
package vcc.dnd4e.view

import model._
import vcc.util.swing.SwingHelper
import vcc.controller.message.{TrackerControlMessage,TransactionalAction}
import scala.actors.Actor

trait ContextObserver {
  def changeContext(nctx:Option[Symbol],isTarget:Boolean)
}

trait PaneDirectorPropertyObserver {
  def propertyChanged(which:PanelDirector.property.Value)
}

object PanelDirector {
  object property extends Enumeration {
    val HideDead = Value("Hide Dead")
  }
}

/**
 * This Mixin is used to help build panels that need to cache and
 * update combatState but that are not interested in the changes or have
 * to react to the change. If a Panel needs to update as soon as state changes
 * do not use this mixin.
 */
trait SimpleCombatStateObserver extends CombatStateObserver {
  
  protected var combatState:CombatState = null
  
  def combatStateChanged(newState:CombatState,changes:CombatStateChanges) {
    combatState = newState
  }
}

/**
 * This compenent act as a Mediator between all the panels and the CombatStateManager.
 */
class PanelDirector(tracker:Actor,csm:CombatStateManager,statusBar:StatusBar) extends CombatStateObserver {

  private var combatState = csm.currentState
  private var combatStateObserver:List[CombatStateObserver] = Nil
  private var contextObserver:List[ContextObserver] = Nil
  private var propertyObserver:List[PaneDirectorPropertyObserver] = Nil
  
  private var propHideDead = false
  
  def currentState = combatState
  
  //Init code
  csm.registerObserver(this)
  
  def combatStateChanged(newState:CombatState,changes:CombatStateChanges) {
    //newState.prettyPrint()
    SwingHelper.invokeInEventDispatchThread {
      combatState = newState
      combatStateObserver.foreach(obs => obs.combatStateChanged(combatState,changes))
    }
  }
  
  def registerContextObserver(obs:ContextObserver) {
	contextObserver = obs :: contextObserver
  }

  def registerStateObserver(obs:CombatStateObserver) {
	combatStateObserver = obs :: combatStateObserver
  }
  
  def registerPropertyObserver(obs:PaneDirectorPropertyObserver) {
	propertyObserver = obs :: propertyObserver
  }
  
  def setActiveCombatant(id:Option[Symbol]) {
    publishContextChange(id,false)
  }
  
  private def publishContextChange(nctx:Option[Symbol],isTarget:Boolean) {
    contextObserver.foreach(obs=>obs.changeContext(nctx,isTarget))
  }
  
  def setTargetCombatant(id:Option[Symbol]) {
    publishContextChange(id,true)
  }
  
  def setStatusBarMessage(text:String) {
    statusBar.setTipText(text)
  }
  
  def setProperty(prop:PanelDirector.property.Value,value:Boolean) {
    if(prop == PanelDirector.property.HideDead) {
      propHideDead = value
    } else {
      throw new Exception("Unknown property: "+prop)
    }
    for(obs <- propertyObserver) obs.propertyChanged(prop)
  }
  
  def getBooleanProperty(prop:PanelDirector.property.Value):Boolean = {
    import PanelDirector.property._
    prop match {
      case HideDead => propHideDead
      case _ => throw new Exception("Unknown property: "+prop)
    }
  }
  
  /**
   * This is the way to request tracker actions. No Panel should
   * send messages directly to the Tracker.
   * @param action A TransactionalAction message
   */
  def requestAction(action:TransactionalAction) {
    tracker ! action
  }

  def requestControllerOperation(action:TrackerControlMessage) {
    tracker ! action
  }
  
}
