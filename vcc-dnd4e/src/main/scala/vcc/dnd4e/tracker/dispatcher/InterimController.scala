/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.tracker.dispatcher

import vcc.dnd4e.tracker.common.CombatState
import vcc.tracker._

class InterimController extends Tracker.Controller[CombatState] {

  private val logger = org.slf4j.LoggerFactory.getLogger("user")
  private val history = new TimeLine[CombatState]

  def clearHistory() {
    history.forgetPast()
  }

  def redo(): Option[CombatState] = {
    if (history.hasFuture) {
      history.forwardState()
      history.getState
    } else
      None
  }

  def undo(): Option[CombatState] = {
    if (history.hasPast) {
      history.revertState()
      history.getState
    } else
      None
  }

  def setInitialState(state: CombatState) {
    history.store(state)
  }

  def dispatchAction(action: Action[CombatState], rulingProvider: RulingProvider[CombatState]): Option[CombatState] = {
    val initialState = history.getState
    if (initialState.isDefined) {
      val dispatcher = ActionDispatcher.getDispatcher(initialState.get)
      dispatcher.setRulingProvider(rulingProvider)
      try {
        dispatcher.handle(action)
        val finalState = dispatcher.resultState
        if (finalState.isDefined)
          history.store(finalState.get)
        return finalState
      } catch {
        case e: Exception =>
          logger.warn("Failed to handle: " + action, e)
      }
    }
    None
  }
}