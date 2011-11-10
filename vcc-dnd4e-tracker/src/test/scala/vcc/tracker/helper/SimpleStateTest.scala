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

class SimpleStateTest extends SpecificationWithJUnit {

  type C = StateCommand[State]

  "actions" should {
    "transalate Init to ResetCommand" in {
      Init(10).createCommandStream() must_== CommandStream(ResetCommand(10))
    }

    "translate Increment to AlterCommand" in {
      Increment(10).createCommandStream() must_== CommandStream(AlterCommand(10))
    }

    "translate Increment to AlterCommand" in {
      Repeat(3, 2).createCommandStream() must_== CommandStream(AlterCommand(2), AlterCommand(2), AlterCommand(2))
    }

    "translate Ask to AskCommand" in {
      Ask("something").createCommandStream() must_== CommandStream(AskCommand("something"))
    }

    "translate NTimeEvent to MultipleTimeCommand" in {
      Multiply(2).createCommandStream() must_== CommandStream(MultiplyCommand(2))
    }

    "translate LoopTo to Sequence builde" in {
      val x = LoopTo(10, 2).createCommandStream()
      x.get(State(9)) must_== Some((AlterCommand(2), x))
      x.get(State(10)) must_== None
      x.get(State(11)) must_== None
    }
  }

  "our AskCommand" should {
    "the ruling locator" in {
      new SimpleRulingLocatorService().
        rulingsFromStateWithCommand(State(0), AskCommand("Prompt")) must_== List(AskValueRuling("Prompt", None))
    }

    "provide ruling" in {
      AskCommand("Prompt").requiredRulings(State(0)) must_== List(AskValueRuling("Prompt", None))
    }
  }

  "our ruling" should {
    "ruling must match" in {
      AskValueRuling("some", None).isRulingSameSubject(AskValueRuling("some", Some(10))) must beTrue
      AskValueRuling("some", None).isRulingSameSubject(AskValueRuling("other", None)) must beFalse
    }

    "ruling must have prompt" in {
      AskValueRuling("Prompt", None).userPrompt(State(11)) must_== "Prompt (Current 11)"
    }

    "provide and anwer and generate events" in {
      val ruling = AskValueRuling("Prompt", None).withDecision(10)
      ruling must_== AskValueRuling("Prompt", Some(10))
      ruling.generateCommands(State(1)) must_== List(ResetCommand(10))
    }

    "provide and anwer and tow generate events on double" in {
      val ruling = AskValueRuling("double", None).withDecision(10)
      ruling.generateCommands(State(1)) must_== List(ResetCommand(10), AlterCommand(10))
    }
  }

  "the commands" should {
    "AlterCommand make a proper set" in {
      AlterCommand(2).generateTransitions(State(12)) must_== List(SetStateEvent(14))
    }

    "ResetCommand make a proper set" in {
      ResetCommand(10).generateTransitions(State(123)) must_== List(SetStateEvent(10))
    }

    "when generate AskCommand return Nil" in {
      AskCommand("some").generateTransitions(State(0)) must_== Nil
    }

    "when generate MultipleCommand return Increment of actions" in {
      MultiplyCommand(0).generateTransitions(State(10)) must_== List(SetStateEvent(0))
      MultiplyCommand(1).generateTransitions(State(10)) must_== Nil
      MultiplyCommand(3).generateTransitions(State(5)) must_==
        List(IncrementEvent(5), IncrementEvent(5))
      MultiplyCommand(-2).generateTransitions(State(10)) must_==
        List(IncrementEvent(-10), IncrementEvent(-10), IncrementEvent(-10))
    }
  }

  "SetStateEvent" should {
    "set the value of state" in {
      SetStateEvent(456).transition(State(123)) must_== State(456)
    }
  }

  "IncrementEvent" should {
    "increment the value of the state" in {
      IncrementEvent(2).transition(State(123)) must_== State(125);
      IncrementEvent(-23).transition(State(123)) must_== State(100);
    }
  }
}