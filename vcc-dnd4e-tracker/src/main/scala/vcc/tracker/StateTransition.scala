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
package vcc.tracker

/**
 * Base trait for commands or operations that transition some state to a new state.
 */
trait StateTransition[S] {
  /**
   * Function to be applied to change state from current value to new value.
   * @param iState Initial state, the transition with take iState to a new state
   * @return New state which results from applying this transition to the state.
   */
  def transition(iState: S): S
}

/**
 * A StateCommand will generate a list of StateTransition based on the current state S of the system
 */
trait StateCommand[S] {
  /**
   * Given a state iState generate the list of StateTransition to move the system from the current
   * @param iState State to use to generate the events
   * @return List of StateTransition
   */
  def changeEvents(iState: S): List[StateTransition[S]]
}

