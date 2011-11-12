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
import helper._
import collection.immutable.List

class ActionDispatcherTest extends SpecificationWithJUnit with Mockito {

  private val actionWithSingleCommandAndEvent = FlexAction(FlexCommand(10))

  trait context extends Scope {
    val startState = State(0)
    val dispatcher = ActionDispatcher.getDispatcher(startState)

    val mockRulingProvider = mock[RulingProvider[State]]
    mockRulingProvider.provideRulingFor(Nil) returns Nil
    dispatcher.setRulingProvider(mockRulingProvider);
  }

  "create handler" in new context {
    dispatcher must not beNull;
  }

  "create new dispatcher from factory method" in {
    val ad = ActionDispatcher.getDispatcher(State(10))
    ad must not beNull;
  }

  "dispatcher should only handle a single dispatch" in new context {
    dispatcher.handle(actionWithSingleCommandAndEvent) must_== State(10)
    dispatcher.handle(actionWithSingleCommandAndEvent) must throwA[IllegalStateException]
  }

  "accept a dispatch of a command" in new context {
    dispatcher.handle(actionWithSingleCommandAndEvent) must_== State(10)
  }

  "dispatch loop action that generates multiple commands" in new context {
    dispatcher.handle(LoopTo(10, 3)) must_== State(12)
  }

  "dectect infinite loop in dispath and commandSteam loop" in new context {
    dispatcher.handle(LoopTo(10, 0)) must throwA[InfiniteLoopException]
  }

  "handle action with single Command and Single Event" in new context {
    dispatcher.handle(FlexAction(FlexCommand(1))) must_== State(1)
  }

  "handle action with single Command and multiple Event" in new context {
    dispatcher.handle(FlexAction(FlexCommand(1, 2, 3))) must_== State(6)
  }

  "handle action with multiple Command each with single Event" in new context {
    dispatcher.handle(FlexAction(FlexCommand(2), FlexCommand(3))) must_== State(5)
  }

  "handle action with multiple Command each with multiple Event" in new context {
    dispatcher.handle(FlexAction(FlexCommand(1, 2, 3), FlexCommand(4, 5))) must_== State(15)
  }

  "handle Action with single Command single Ruling that results int one Command" in new context {
    mockRulingProvider.provideRulingFor(List(FlexRuling("what", None))) returns
      List(FlexRuling("what", makeDecisionList(FlexCommand(14))))

    dispatcher.handle(FlexAction(FlexCommand("what"))) must_== State(14)
    there was one(mockRulingProvider).provideRulingFor(List(FlexRuling("what", None)))
  }

  private implicit def int2IncrementEvent(increment: Int): Event[State] = IncrementEvent(increment)

  private def makeDecisionList(decisions: Command[State]*): Some[List[Command[State]]] = {
    Some(decisions.toList)
  }
}