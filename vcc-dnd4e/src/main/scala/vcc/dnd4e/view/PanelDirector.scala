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

import helper.{RobinHeadFirstInitiativeOrderViewBuilder, SortedIDReserveViewBuilder, DirectInitiativeOrderViewBuilder}
import vcc.dnd4e.domain.tracker.common._
import vcc.util.swing.SwingHelper
import vcc.controller.message.{TrackerControlMessage, TransactionalAction}
import scala.actors.Actor
import vcc.controller.message.Command
import vcc.controller._
import vcc.infra.prompter.RulingBroker

trait ContextObserver {
  def changeTargetContext(newContext: Option[UnifiedCombatantID]) {}

  def changeSourceContext(newContext: Option[UnifiedCombatantID]) {}
}

trait PaneDirectorPropertyObserver {
  def propertyChanged(which: PanelDirector.property.Value)
}

object PanelDirector {

  object property extends Enumeration {
    val HideDead = Value("Hide Dead")
    val RobinView = Value("Robin View")
  }
}

trait CombatStateObserver {
  def combatStateChanged(newState: UnifiedSequenceTable)
}

/**
 * This Mixin is used to help build panels that need to cache and
 * update combatState but that are not interested in the changes or have
 * to react to the change. If a Panel needs to update as soon as state changes
 * do not use this mixin.
 */
trait SimpleCombatStateObserver extends CombatStateObserver {
  protected var combatState: UnifiedSequenceTable = null

  def combatStateChanged(newState: UnifiedSequenceTable) {
    combatState = newState
  }
}

/**
 * This component act as a Mediator between all the panels and the CombatStateManager.
 */
class PanelDirector(tracker: Actor, csm: TrackerChangeObserver[CombatStateView], statusBar: StatusBar, rulingBroker: RulingBroker)
  extends TrackerChangeAware[CombatStateView] with CommandSource {
  private var combatStateObserver: List[CombatStateObserver] = Nil
  private var contextObserver: List[ContextObserver] = Nil
  private var propertyObserver: List[PaneDirectorPropertyObserver] = Nil

  private var propHideDead = false

  private var propRobinView = true

  private var unifiedTable = new UnifiedSequenceTable(Array(), csm.getSnapshot())

  val rules = new CombatStateRules()

  def currentState = unifiedTable

  private val logger = org.slf4j.LoggerFactory.getLogger("user")

  //Init code
  csm.addChangeObserver(this)

  def snapshotChanged(newState: CombatStateView) {
    SwingHelper.invokeInEventDispatchThread {
      unifiedTable = UnifiedSequenceTable.buildList(newState,
        if (propRobinView) RobinHeadFirstInitiativeOrderViewBuilder else DirectInitiativeOrderViewBuilder,
        SortedIDReserveViewBuilder)
      combatStateObserver.foreach(obs => obs.combatStateChanged(unifiedTable))
    }
  }

  def registerContextObserver(obs: ContextObserver) {
    contextObserver = obs :: contextObserver
  }

  def registerStateObserver(obs: CombatStateObserver) {
    combatStateObserver = obs :: combatStateObserver
  }

  def registerPropertyObserver(obs: PaneDirectorPropertyObserver) {
    propertyObserver = obs :: propertyObserver
  }

  def setActiveCombatant(id: Option[UnifiedCombatantID]) {
    contextObserver.foreach(obs => obs.changeSourceContext(id))
  }

  def setTargetCombatant(id: Option[UnifiedCombatantID]) {
    contextObserver.foreach(obs => obs.changeTargetContext(id))
  }

  def setStatusBarMessage(text: String) {
    statusBar.setTipText(text)
  }

  def setProperty(prop: PanelDirector.property.Value, value: Boolean) {
    prop match {
      case PanelDirector.property.HideDead =>
        propHideDead = value
      case PanelDirector.property.RobinView =>
        propRobinView = value
        //Need to update sequence
        snapshotChanged(csm.getSnapshot())
      case _ =>
        throw new Exception("Unknown property: " + prop)
    }
    for (obs <- propertyObserver) obs.propertyChanged(prop)
  }

  def getBooleanProperty(prop: PanelDirector.property.Value): Boolean = {
    import PanelDirector.property._
    prop match {
      case HideDead => propHideDead
      case RobinView => propRobinView
      case _ => throw new Exception("Unknown property: " + prop)
    }
  }

  /**
   * This is the way to request tracker actions. No Panel should
   * send messages directly to the Tracker.
   * @param action A TransactionalAction message
   */
  def requestAction(action: TransactionalAction) {
    tracker ! Command(this, action)
  }

  def requestControllerOperation(action: TrackerControlMessage) {
    tracker ! action
  }

  def actionCompleted(msg: String, producedChanges: Boolean) {
    logger.info("Executed: " + msg)
  }

  def actionCancelled(reason: String) {
    logger.warn("Failed to execute action, reason: " + reason)
  }

  /**
   * Send a request to get user input on ruling.
   * @param context
   * @param rulings A List of Ruling that require some user Decision
   * @return A list of Decision in order according to the rulings
   */
  def provideDecisionsForRulings(context: TransactionalAction, rulings: List[Ruling]): List[Decision[_ <: Ruling]] = {
    if (rulings.isEmpty) Nil
    else rulingBroker.promptRuling("NO-MESSAGE-MIGRATING", rulings)
  }
}