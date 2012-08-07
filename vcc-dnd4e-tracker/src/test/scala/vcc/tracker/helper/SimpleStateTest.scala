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

class SimpleStateTest extends SpecificationWithJUnit {

  "LoopToAction" should {
    "translate LoopTo to conditional CommandStream" in {
      val commandStream = LoopToAction(10, 2).createCommandStream()
      commandStream.get(State(9)) must_== Some((FlexCommand(IncrementEvent(2)), commandStream))
      commandStream.get(State(10)) must_== None
      commandStream.get(State(11)) must_== None
    }
  }

  "FlexCommand" should {
    "ask for ruling when it has prompt parameter" in {
      FlexCommand("what", IncrementEvent(1)).requiredRulings(State(10)) must_== List(FlexRuling("what", None))
    }

    "not ask for ruling when it has no prompt" in {
      FlexCommand(IncrementEvent(1)).requiredRulings(State(10)) must_== Nil
    }

    "ask for all rulings it has multiple prompts" in {
      FlexCommand(List("what", "where"), List(IncrementEvent(1))).requiredRulings(State(10)) must_==
        List(FlexRuling("what", None), FlexRuling("where", None))
    }

    "generate events for each of parameters" in {
      FlexCommand(IncrementEvent(1), IncrementEvent(2)).generateEvents(State(1)) must_==
        List(IncrementEvent(1), IncrementEvent(2))
    }
  }

  "CrashCommand" should {
    "throw exception when generating Event" in {
      CrashCommand("boom").generateEvents(State(7)) must throwA[Exception]
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

  "CrashEvent" should {
    "throw exception when executing transitions" in {
      CrashEvent("some").transition(State(32)) must throwA[IllegalStateException]
    }
  }
}