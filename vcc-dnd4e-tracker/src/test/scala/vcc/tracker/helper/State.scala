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
package vcc.tracker.helper

import vcc.tracker._

case class State(value: Int)

sealed trait Action

case class Init(v: Int) extends Action

case class Increment(by: Int) extends Action

case class LoopTo(limit: Int, step: Int) extends Action

case class Repeat(time: Int, action: Int) extends Action

case class Ask(prompt: String) extends Action

case class Multiply(times: Int) extends Action


case class ResetCommand(newStateValue: Int) extends StateCommand[State] {
  def generateTransitions(iState: State): List[StateTransition[State]] = List(SetStateEvent(newStateValue))
}

case class AlterCommand(delta: Int) extends StateCommand[State] {
  def generateTransitions(iState: State): List[StateTransition[State]] = List(SetStateEvent(iState.value + delta))
}

case class AskCommand(whatToAsk: String) extends StateCommand[State] {
  def generateTransitions(iState: State): List[StateTransition[State]] = Nil
}

case class MultiplyCommand(time: Int) extends StateCommand[State] {
  def generateTransitions(iState: State): List[StateTransition[State]] = {
    time match {
      case 0 => List(SetStateEvent(0))
      case i if (i < 0) => makeIncrementList(-time, -iState.value)
      case i => makeIncrementList(time - 2, iState.value)
    }
  }

  private def makeIncrementList(n: Int, factor: Int): scala.List[IncrementEvent] = {
    (0 to n).map(i => IncrementEvent(factor)).toList
  }
}

case class SetStateEvent(value: Int) extends StateTransition[State] {
  def transition(iState: State): State = State(value)
}

case class IncrementEvent(inc: Int) extends StateTransition[State] {
  def transition(iState: State): State = State(iState.value + inc)
}

case class AskValueRuling(prompt: String, decision: Option[Int]) extends Ruling[State, Int, AskValueRuling] {

  def isRulingSameSubject(otherRuling: Ruling[State, _, _]): Boolean = {
    otherRuling match {
      case AskValueRuling(otherPrompt, _) => this.prompt == otherPrompt
      case _ => false
    }
  }

  def userPrompt(state: State): String = prompt + " (Current " + state.value + ")"

  protected def commandsFromDecision(state: State): List[StateCommand[State]] = {
    if (prompt == "double")
      List(ResetCommand(decision.get), AlterCommand(decision.get))
    else
      List(ResetCommand(decision.get))
  }

  def withDecision(decision: Int): AskValueRuling = copy(decision = Some(decision))
}

class Translator extends ActionStreamTranslator[State, Action] {
  def translateToCommandStream(action: Action): CommandStream[State, StateCommand[State]] = {
    action match {
      case Init(v) => CommandStream(ResetCommand(v))
      case Increment(by) => CommandStream(AlterCommand(by))
      case Repeat(times, amount) => SeqCommandStream(Seq.fill(times)(AlterCommand(amount)))
      case LoopTo(limit, step) => new PartialFunctionCommandStream[State, StateCommand[State]]({
        case State(current) if (current < limit) => AlterCommand(step)
      })
      case Ask(prompt) => CommandStream(AskCommand(prompt))
      case Multiply(times) => CommandStream(MultiplyCommand(times))
    }
  }
}

class SimpleRulingLocatorService extends RulingLocationService[State] {
  def rulingsFromStateWithCommand(state: State, command: StateCommand[State]): List[Ruling[State, _, _]] = {
    command match {
      case AskCommand(prompt) => List(AskValueRuling(prompt, None))
      case _ => Nil
    }
  }
}