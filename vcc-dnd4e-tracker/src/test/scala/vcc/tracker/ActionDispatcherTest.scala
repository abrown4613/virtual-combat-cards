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

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import org.specs2.mock.Mockito

class ActionDispatcherTest extends SpecificationWithJUnit with Mockito {

  import helper._

  trait context extends Scope {
    val mockTranslator = spy(new Translator)
    val mockRulingLocator = spy(new SimpleRulingLocatorService)
    val mockRulingProvider = mock[RulingProvider[State]]
    val dispatcher = new ActionDispatcher[State, Action](mockTranslator, mockRulingLocator)
    val startState = State(0)

    mockRulingProvider.provideRulingFor(Nil) returns Nil
    dispatcher.setRulingProvider(mockRulingProvider);
  }

  "create handler" in new context {
    dispatcher must not beNull;
  }

  "translate action to command" in new context {
    dispatcher.handle(startState, Init(10))
    there was one(mockTranslator).translateToCommandStream(Init(10))
  }

  "accept a dispatch of a command" in new context {
    dispatcher.handle(startState, Init(10)) must_== State(10)
  }

  "dispatch action that generates multiple commands" in new context {
    dispatcher.handle(startState, Repeat(3, 2)) must_== State(6)
  }

  "dispatch loop action that generates multiple commands" in new context {
    dispatcher.handle(startState, LoopTo(10, 3)) must_== State(12)
  }

  "process all events generated from a Multiply action" in new context {
    dispatcher.handle(State(10), Multiply(3)) must_== State(30)
  }

  "dectect infinite loop in dispath and commandSteam loop" in new context {
    dispatcher.handle(startState, LoopTo(10, 0)) must throwA[InfiniteLoopException]
  }

  "ask for possible ruling on every message" in new context {
    dispatcher.handle(startState, LoopTo(4, 2))
    there was one(mockRulingLocator).rulingsFromStateWithCommand(startState, AlterCommand(2)) then
      one(mockRulingLocator).rulingsFromStateWithCommand(State(2), AlterCommand(2))
  }

  "ask RulingProvider for ruling if a ruling is needed" in new context {
    mockRulingProvider.provideRulingFor(List(AskValueRuling("some", None))) returns List(AskValueRuling("some", Some(14)))

    dispatcher.handle(startState, Ask("some"))
    there was one(mockRulingProvider).provideRulingFor(List(AskValueRuling("some", None)))
  }

  "handle Rulings provided" in new context {
    mockRulingProvider.provideRulingFor(List(AskValueRuling("some", None))) returns List(AskValueRuling("some", Some(14)))
    dispatcher.handle(startState, Ask("some")) must_== State(14)
  }

  "handle all Rulings provided" in new context {
    mockRulingProvider.provideRulingFor(List(AskValueRuling("double", None))) returns List(AskValueRuling("double", Some(14)))
    dispatcher.handle(startState, Ask("double")) must_== State(28)
  }

}