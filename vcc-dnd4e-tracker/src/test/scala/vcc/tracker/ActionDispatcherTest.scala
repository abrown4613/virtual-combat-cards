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
import java.lang.String

class ActionDispatcherTest extends SpecificationWithJUnit with Mockito {

  private val actionWithSingleCommandAndEvent = FlexAction(FlexCommand(10))

  trait context extends Scope {
    val startState = State(0)
    val dispatcher = ActionDispatcher.getDispatcher(startState)

    val mockRulingProvider = mock[RulingProvider[State]]
    mockRulingProvider.provideRulingFor(any, any) returns Nil
    dispatcher.setRulingProvider(mockRulingProvider);

    protected def mockFlexRulingDecision(state: State, decisions: flexDecision*) {
      val expectedRulings: List[FlexRuling] = decisions.map(d => FlexRuling(d.prompt, None)).toList
      val providedDecisions = decisions.map(d => FlexRuling(d.prompt, Some(d.decisionCommands.toList))).toList

      mockRulingProvider.provideRulingFor(state, expectedRulings) returns providedDecisions
    }

    protected case class flexDecision(prompt: String, decisionCommands: Command[State]*)

  }

  "create handler" in new context {
    dispatcher must not beNull;
  }

  "create new dispatcher from factory method" in {
    val ad = ActionDispatcher.getDispatcher(State(10))
    ad must not beNull;
  }

  "dispatcher should only handle a single dispatch" in new context {
    dispatcher.handle(actionWithSingleCommandAndEvent)
    dispatcher.handle(actionWithSingleCommandAndEvent) must throwA[IllegalStateException]
  }

  "accept a dispatch of a command" in new context {
    dispatcher.handle(actionWithSingleCommandAndEvent)
    dispatcher.resultState.get must_== State(10)
  }

  "dispatch loop action that generates multiple commands" in new context {
    dispatcher.handle(LoopTo(10, 3))
    dispatcher.resultState.get must_== State(12)
  }

  "detect infinite loop in dispath and commandSteam loop" in new context {
    dispatcher.handle(LoopTo(10, 0)) must throwA[InfiniteLoopException]
  }

  "handle action with single Command and Single Event" in new context {
    dispatcher.handle(FlexAction(FlexCommand(1)))
    dispatcher.resultState.get must_== State(1)
  }

  "handle action with single Command and multiple Event" in new context {
    dispatcher.handle(FlexAction(FlexCommand(1, 2, 3)))
    dispatcher.resultState.get must_== State(6)
  }

  "handle action with multiple Command each with single Event" in new context {
    dispatcher.handle(FlexAction(FlexCommand(2), FlexCommand(3)))
    dispatcher.resultState.get must_== State(5)
  }

  "handle action with multiple Command each with multiple Event" in new context {
    dispatcher.handle(FlexAction(FlexCommand(1, 2, 3), FlexCommand(4, 5)))
    dispatcher.resultState.get must_== State(15)
  }

  "handle Action with single Command single Ruling that results in one Command" in new context {
    mockFlexRulingDecision(startState, flexDecision("what", FlexCommand(14)))

    dispatcher.handle(FlexAction(FlexCommand("what")))

    dispatcher.resultState.get must_== State(14)
    there was one(mockRulingProvider).provideRulingFor(startState, List(FlexRuling("what", None)))
  }

  "handle Action with single Command single Ruling that results in one Command follow by original events" in new context {
    mockFlexRulingDecision(startState, flexDecision("what", FlexCommand(SetStateEvent(5))))

    dispatcher.handle(FlexAction(FlexCommand("what", 1, 1)))

    dispatcher.resultState.get must_== State(7)
    there was one(mockRulingProvider).provideRulingFor(startState, List(FlexRuling("what", None)))
  }

  "handle Action with single Command that produces several Ruling with multiple commands of multiple events" in new context {
    mockFlexRulingDecision(startState,
      flexDecision("what", FlexCommand(1), FlexCommand(2, 3)),
      flexDecision("where", FlexCommand(4), FlexCommand(5, 6)))

    dispatcher.handle(FlexAction(new FlexCommand(List("what", "where"), List(7, 8))))

    dispatcher.resultState.get must_== State(36)
  }

  "handle Action with two Commands that produces single Ruling with multiple commands of multiple events" in new context {
    mockFlexRulingDecision(startState, flexDecision("what", FlexCommand(1, 2)))
    mockFlexRulingDecision(State(3), flexDecision("where", FlexCommand(3, 4)))

    dispatcher.handle(FlexAction(FlexCommand("what"), FlexCommand("where", 5)))

    dispatcher.resultState.get must_== State(15)
  }

  private implicit def int2IncrementEvent(increment: Int): Event[State] = IncrementEvent(increment)
}