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

import org.specs2.mutable.SpecificationWithJUnit
import vcc.tracker._

case class State(value: Int)

sealed trait Action

case class Init(v: Int) extends Action

case class Increment(by: Int) extends Action

case class LoopTo(limit: Int, step: Int) extends Action

case class Repeat(time: Int, action: Int) extends Action

case class ResetCommand(newStateValue: Int) extends StateCommand[State] {
  def generateTransitions(iState: State): List[StateTransition[State]] = List(SetStateEvent(newStateValue))
}

case class AlterCommand(delta: Int) extends StateCommand[State] {
  def generateTransitions(iState: State): List[StateTransition[State]] = List(SetStateEvent(iState.value + delta))
}

case class SetStateEvent(value: Int) extends StateTransition[State] {
  def transition(iState: State): State = State(value)
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
    }
  }
}

class SimpleStateTest extends SpecificationWithJUnit {

  type C = StateCommand[State]


  "the translator" should {
    "transalate Init to ResetCommand" in {
      new Translator().translateToCommandStream(Init(10)) must_== CommandStream(ResetCommand(10))
    }

    "translate Increment to AlterCommand" in {
      new Translator().translateToCommandStream(Increment(10)) must_== CommandStream(AlterCommand(10))
    }

    "translate Increment to AlterCommand" in {
      new Translator().translateToCommandStream(Repeat(3, 2)) must_== CommandStream(
        AlterCommand(2), AlterCommand(2), AlterCommand(2))
    }

    "translate LoopTo to Sequence builde" in {
      val x = new Translator().translateToCommandStream(LoopTo(10, 2))
      x.get(State(9)) must_== Some((AlterCommand(2), x))
      x.get(State(10)) must_== None
      x.get(State(11)) must_== None
    }
  }

  "the commands" should {
    "AlterCommand make a proper set" in {
      AlterCommand(2).generateTransitions(State(12)) must_== List(SetStateEvent(14))
    }

    "ResetCommand make a proper set" in {
      ResetCommand(10).generateTransitions(State(123)) must_== List(SetStateEvent(10))
    }
  }

  "SetStateEvent" should {
     "set the value of state" in {
       SetStateEvent(456).transition(State(123)) must_== State(456)
     }
  }
}






