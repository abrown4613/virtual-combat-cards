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
//$Id$
package vcc.dnd4e.tracker.transition

import vcc.dnd4e.tracker.common.CombatState
import vcc.dnd4e.tracker.{StateLensFactory, LensFactory}

/**
 * Base trait for commands or operations that transition some state to a new state.
 */
trait StateTransition[S, L <: LensFactory[S]] {
  /**
   * Function to be applied to change state from current value to new value.
   * @param lf Lens factory to be used by state transition operations. Use is optional, but provides easier mocking and
   * isolation of concerns.
   * @param iState Initial state, the transition with take iState to a new state
   * @return New state which results from applying this transition to the state.
   */
  def transition(lf: L, iState: S): S
}

trait CombatTransition extends StateTransition[CombatState, StateLensFactory]
