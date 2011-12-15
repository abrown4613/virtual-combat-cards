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

import vcc.dnd4e.domain.tracker.common._
import vcc.util.swing.SwingHelper
import vcc.dnd4e.tracker.common.CombatState
import vcc.dnd4e.tracker.common.Command.CombatStateAction
import vcc.dnd4e.tracker.dispatcher.CombatStateViewAdapterBuilder
import vcc.tracker.Tracker
import vcc.dnd4e.application.{CombatSaveFile, Application}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

trait ContextObserver {
  def changeTargetContext(newContext: Option[UnifiedCombatantID]) {}

  def changeSourceContext(newContext: Option[UnifiedCombatantID]) {}
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
class PanelDirector(tracker: Tracker[CombatState], statusBar: StatusBar) {
  private var combatStateObserver: List[CombatStateObserver] = Nil
  private var contextObserver: List[ContextObserver] = Nil
  private var propHideDead = false
  private var propRobinView = true
  private var unifiedTable: UnifiedSequenceTable = null
  private val sequenceBuilder = new UnifiedSequenceTable.Builder()
  private val rulingProvider = new DialogRulingProvider()
  val rules = new CombatStateRules()

  private val logger = org.slf4j.LoggerFactory.getLogger("user")

  Application.initialize(tracker, new Tracker.Observer[CombatState] {
    def stateUpdated(state: CombatState) {
      logger.debug("[NT] -> " + state)
      val newState = CombatStateViewAdapterBuilder.buildView(state)
      buildStateAndPropagate(newState)
      testSaveFile(state)
    }

    def testSaveFile(state: CombatState) {
      try {
        val saveFile = new CombatSaveFile()
        val os = new ByteArrayOutputStream()
        saveFile.save(os, state)
        val loadedState = saveFile.load(new ByteArrayInputStream(os.toByteArray))
        if (! (state == loadedState)) {
          logger.warn("Serialization failed for state...")
          logger.debug("Expected: " + state)
          logger.debug("  Loaded: " + loadedState)
          logger.debug("  -  Roster equal: " + (state.roster==loadedState.roster))
          logger.debug("  -   Order equal: " + (state.order==loadedState.order))
          logger.debug("  - Comment equal: " + (state.comment==loadedState.comment))
        }
      } catch {
        case e =>
          logger.warn("Failed save and load operation", e)
      }
    }
  })

  def registerContextObserver(obs: ContextObserver) {
    contextObserver = obs :: contextObserver
  }

  def registerStateObserver(obs: CombatStateObserver) {
    combatStateObserver = obs :: combatStateObserver
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

  def setProperty(property: PanelDirector.property.Value, value: Boolean) {
    property match {
      case PanelDirector.property.HideDead =>
        propHideDead = value
        if (value) sequenceBuilder.hideDead()
        else sequenceBuilder.showDead()
      case PanelDirector.property.RobinView =>
        propRobinView = value
        if (value) sequenceBuilder.useRobinOrder()
        else sequenceBuilder.useDirectOrder()
      case _ =>
        throw new Exception("Unknown property: " + property)
    }
    buildStateAndPropagate(unifiedTable.state)
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
  def requestAction(action: CombatStateAction) {
    tracker.dispatchAction(action, rulingProvider)
  }

  def requestClearHistory() {
    tracker.clearHistory()
  }

  def requestUndo() {
    tracker.undo()
  }

  def requestRedo() {
    tracker.redo()
  }

  def actionCompleted(msg: String, producedChanges: Boolean) {
    logger.info("Executed: " + msg)
  }

  def actionCancelled(reason: String) {
    logger.warn("Failed to execute action, reason: " + reason)
  }

  private def buildStateAndPropagate(newState: CombatStateView) {
    SwingHelper.invokeInEventDispatchThread {
      unifiedTable = sequenceBuilder.build(newState)
      combatStateObserver.foreach(obs => obs.combatStateChanged(unifiedTable))
    }
  }
}