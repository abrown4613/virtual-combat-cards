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

class ExecutorTest extends SpecificationWithJUnit {

  "A Executor" should {
    "be well behaved" in {
      success
    }
  }

  case class IntEvent(value: Int) extends StateTransition[Int] {
    def transition(iState: Int): Int = iState + value
  }

  case class IntAction(value: Int) extends StateCommand[Int] {
    def generateTransitions(iState: Int): List[StateTransition[Int]] = null
  }

  class Trans extends ActionStreamTranslator[Int, String] {
    def translateToCommandStream(action: String): CommandStream[Int, StateCommand[Int]] = {
      new SeqCommandStream[Int, StateCommand[Int]](Seq(IntAction(action.toInt)))
    }
  }

  val debugExecutor = new ActionExecutor[Int, String](
    new Trans,
    new StateCommandDispatcher[Int](new RulingDispatcher[Int](
      new RulingPeer[Int] {
        def provideDecisionForRuling(state: Int, rulings: List[Ruling[Int, _, _, _]]): List[Ruling[Int, _, _, _]] = Nil
      },
      new RulingLocationService[Int] {
        def rulingsFromStateWithCommand(state: Int, command: StateCommand[Int]): List[Ruling[Int, _, _, _]] = Nil
      }
    )),
    new DebugTransitionBuilder[Int, String](new AccumulatorTransitionBuilder[Int, String], null))
}