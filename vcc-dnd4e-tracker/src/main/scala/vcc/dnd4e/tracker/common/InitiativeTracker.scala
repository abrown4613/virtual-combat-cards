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

object InitiativeAction extends Enumeration {
  val StartRound = Value("Start Round")
  val EndRound = Value("End Round")
  val MoveUp = Value("Move Up")
  val DelayAction = Value("Delay")
  val ReadyAction = Value("Ready Action")
  val ExecuteReady = Value("Execute Ready")
}

object InitiativeState extends Enumeration {
  val Ready = Value("Ready")
  val Readying = Value("Readying")
  // This state is when the combatant has readied but not ended it's round
  val Delaying = Value("Delaying")
  val Acting = Value("Acting")
  val Waiting = Value("Waiting")
}

object InitiativeTracker {

  import InitiativeState._
  import InitiativeAction._

  def initialTracker(orderID: InitiativeOrderID, roll: Int): InitiativeTracker = InitiativeTracker(orderID, 0, roll, Waiting)

  val transformation: PartialFunction[(InitiativeTracker, InitiativeTracker, InitiativeAction.Value), InitiativeTracker] = {
    case (it, first, StartRound) if (it.orderID == first.orderID && (it.state == Waiting || it.state == Ready)) =>
      it.copy(round = it.round + 1, state = Acting)
    case (it, first, EndRound) if (it.orderID == first.orderID && it.state == Acting) =>
      it.copy(state = Waiting)
    case (it, first, EndRound) if (it.orderID == first.orderID && it.state == Readying) =>
      it.copy(state = Ready)
    case (it, first, EndRound) if (it.orderID == first.orderID && it.state == Delaying) =>
      it.copy(state = Waiting)
    case (it, first, MoveUp) if (first.orderID != it.orderID && first.state != Acting && first.state != Readying && it.state == Delaying) =>
      it.copy(state = Acting)
    case (it, first, ReadyAction) if (first.orderID == it.orderID && it.state == Acting) =>
      it.copy(state = Readying)
    case (it, first, ExecuteReady) if (it.orderID != first.orderID && first.state == Acting && it.state == Ready) =>
      it.copy(state = Waiting)
    case (it, first, DelayAction) if (first.orderID == it.orderID && it.state == Acting) =>
      it.copy(state = Delaying)
  }
}

/**
 * Tracks the initiative state of an InitiativeOrderID.
 * @param orderID The initiative order ID being tracked
 * @param round The round in which that InitiativeOrder is, used for traking number of action takes
 * @param state Current InitiativeState
 */
case class InitiativeTracker(orderID: InitiativeOrderID, round: Int, initScore: Int, state: InitiativeState.Value) extends CombatantAspect {

  /**
   * Indicate it a given transformation can be applied.
   * @param first Combatant is the first in sequence?
   * @param action Initiative action to be executed
   * @return True if the transformation is valid
   */
  def canTransform(first: InitiativeTracker, action: InitiativeAction.Value): Boolean = InitiativeTracker.transformation.isDefinedAt(this, first, action)

  /**
   * Indicate it a given transformation can be applied.
   * @param first Combatant is the first in sequence?
   * @param action Initiative action to be executed
   * @return The new InitiativeTracker with changes applied
   */
  def transform(first: InitiativeTracker, action: InitiativeAction.Value): InitiativeTracker = InitiativeTracker.transformation(this, first, action)

  //FIXME
  private def copyAtNextRound(state: InitiativeState.Value) = InitiativeTracker(this.orderID, this.round + 1, this.initScore, state)

  //FIXME
  private def copyAtSameRound(state: InitiativeState.Value) = InitiativeTracker(this.orderID, this.round, this.initScore, state)

}
