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
package vcc.dnd4e.tracker.common

object InitiativeTracker {
  def initialTracker(orderID: InitiativeOrderID, roll: Int): InitiativeTracker = InitiativeTracker(orderID, 0, roll, state.Waiting)

  object action extends Enumeration {
    val StartRound = Value("Start Round")
    val EndRound = Value("End Round")
    val MoveUp = Value("Move Up")
    val Delay = Value("Delay")
    val Ready = Value("Ready Action")
    val ExecuteReady = Value("Execute Ready")
  }

  object state extends Enumeration {
    val Ready = Value("Ready")
    val Readying = Value("Readying")
    // This state is when the combatant has readied but not ended it's round
    val Delaying = Value("Delaying")
    val Acting = Value("Acting")
    val Waiting = Value("Waiting")
  }


  val transformation: PartialFunction[(InitiativeTracker, InitiativeTracker, action.Value), InitiativeTracker] = {
    case (it, first, action.StartRound) if (it.orderID == first.orderID && (it.state == state.Waiting || it.state == state.Ready)) =>
      it.copyAtNextRound(state.Acting)
    case (it, first, action.EndRound) if (it.orderID == first.orderID && it.state == state.Acting) =>
      it.copyAtSameRound(state.Waiting)
    case (it, first, action.EndRound) if (it.orderID == first.orderID && it.state == state.Readying) =>
      it.copyAtSameRound(state.Ready)
    case (it, first, action.EndRound) if (it.orderID == first.orderID && it.state == state.Delaying) =>
      it.copyAtSameRound(state.Waiting)
    case (it, first, action.MoveUp) if (first.orderID != it.orderID && first.state != state.Acting && first.state != state.Readying && it.state == state.Delaying) =>
      it.copyAtSameRound(state.Acting)
    case (it, first, action.Ready) if (first.orderID == it.orderID && it.state == state.Acting) =>
      it.copyAtSameRound(state.Readying)
    case (it, first, action.ExecuteReady) if (it.orderID != first.orderID && first.state == state.Acting && it.state == state.Ready) =>
      it.copyAtSameRound(state.Waiting)
    case (it, first, action.Delay) if (first.orderID == it.orderID && it.state == state.Acting) =>
      it.copyAtSameRound(state.Delaying)
  }
}

/**
 * Tracks the initiative state of an InitiativeOrderID.
 * @param orderID The initiative order ID being tracked
 * @param round The round in which that InitiativeOrder is, used for traking number of action takes
 * @param state Current InitiativeState
 */
case class InitiativeTracker(orderID: InitiativeOrderID, round: Int, initScore: Int, state: InitiativeTracker.state.Value) extends CombatantAspect {

  /**
   * Indicate it a given transformation can be applied.
   * @param first Combatant is the first in sequence?
   * @param action Initiative action to be executed
   * @return True if the transformation is valid
   */
  def canTransform(first: InitiativeTracker, action: InitiativeTracker.action.Value): Boolean = InitiativeTracker.transformation.isDefinedAt(this, first, action)

  /**
   * Indicate it a given transformation can be applied.
   * @param first Combatant is the first in sequence?
   * @param action Initiative action to be executed
   * @return The new InitiativeTracker with changes applied
   */
  def transform(first: InitiativeTracker, action: InitiativeTracker.action.Value): InitiativeTracker = InitiativeTracker.transformation(this, first, action)

  private def copyAtNextRound(state: InitiativeTracker.state.Value) = InitiativeTracker(this.orderID, this.round + 1, this.initScore, state)

  private def copyAtSameRound(state: InitiativeTracker.state.Value) = InitiativeTracker(this.orderID, this.round, this.initScore, state)

}
