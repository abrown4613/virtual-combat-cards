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
    "translate LoopTo to Sequence builde" in {
      val x = LoopTo(10, 2).createCommandStream()
      x.get(State(9)) must_== Some((FlexCommand(IncrementEvent(2)), x))
      x.get(State(10)) must_== None
      x.get(State(11)) must_== None
    }

    "FlexCommand translates to series of comamnds" in {
      val command1 = FlexCommand(IncrementEvent(1))
      val command2 = FlexCommand(SetStateEvent(4))
      FlexAction(command1, command2).createCommandStream() must_== CommandStream(command1, command2)
    }
  }

  "our AskCommand" should {
    "ask for ruling when FlexCommand has prompt parameter" in {
      FlexCommand("what", IncrementEvent(1)).requiredRulings(State(10)) must_== List(FlexRuling("what", None))
    }

    "not ask for ruling when FlexCommand if prompt parameter is null" in {
      FlexCommand(IncrementEvent(1)).requiredRulings(State(10)) must_== Nil
    }

    "ask for all rulings when FlexCommand has multiple prompts" in {
      FlexCommand(List("what", "where"), List(IncrementEvent(1))).requiredRulings(State(10)) must_==
        List(FlexRuling("what", None), FlexRuling("where", None))
    }
  }

  "the commands" should {
    "when generate FlexCommand(a,b) generate events a, b" in {
      FlexCommand(IncrementEvent(1), IncrementEvent(2)).generateEvents(State(1)) must_==
        List(IncrementEvent(1), IncrementEvent(2))
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