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

case class Init(v: Int) extends Action[State] {
  def createCommandStream(): CommandStream[State,Command[State]] = singleCommand(ResetCommand(v))
}

case class Increment(by: Int) extends Action[State] {
  def createCommandStream(): CommandStream[State,Command[State]] = singleCommand(AlterCommand(by))
}

case class LoopTo(limit: Int, step: Int) extends Action[State] {
  def createCommandStream(): CommandStream[State,Command[State]] = {
     new PartialFunctionCommandStream[State, Command[State]]({
            case State(current) if (current < limit) => AlterCommand(step)
          })
  }
}

case class Repeat(times: Int, amount: Int) extends Action[State] {
  def createCommandStream(): CommandStream[State,Command[State]] = {
     SeqCommandStream(Seq.fill(times)(AlterCommand(amount)))
  }
}

case class Ask(prompt: String) extends Action[State] {
  def createCommandStream(): CommandStream[State,Command[State]] = singleCommand(AskCommand(prompt))
}

case class Multiply(times: Int) extends Action[State] {
  def createCommandStream(): CommandStream[State,Command[State]] = singleCommand(MultiplyCommand(times))
}

case class ResetCommand(newStateValue: Int) extends Command[State] {
  def generateTransitions(iState: State): List[StateTransition[State]] = Nil

  override def generateEvents(state:State):List[Event[State]] = List(SetStateEvent(newStateValue))
}

case class AlterCommand(delta: Int) extends Command[State] {
  def generateTransitions(iState: State): List[StateTransition[State]] = Nil

  override def generateEvents(state: State): List[Event[State]] = List(IncrementEvent(delta))
}

case class AskCommand(whatToAsk: String) extends Command[State] {
  def generateTransitions(iState: State): List[StateTransition[State]] = Nil

  override def requiredRulings(state: State): List[Ruling[State, _, _]] = List(AskValueRuling(whatToAsk, None))
}

case class MultiplyCommand(time: Int) extends Command[State] {
  def generateTransitions(iState: State): List[StateTransition[State]] = Nil

  override def generateEvents(iState: State): List[Event[State]] = {
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

case class SetStateEvent(value: Int) extends StateTransition[State] with Event[State] {
  def transition(iState: State): State = State(value)
}

case class IncrementEvent(inc: Int) extends StateTransition[State] with Event[State] {
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

  protected def commandsFromDecision(state: State): List[Command[State]] = {
    if (prompt == "double")
      List(ResetCommand(decision.get), AlterCommand(decision.get))
    else
      List(ResetCommand(decision.get))
  }

  def withDecision(decision: Int): AskValueRuling = copy(decision = Some(decision))
}