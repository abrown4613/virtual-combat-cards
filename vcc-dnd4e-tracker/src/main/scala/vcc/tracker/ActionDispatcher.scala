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

class InfiniteLoopException extends RuntimeException

class ActionDispatcher[S, A](translator: ActionStreamTranslator[S, A]) {

  private var returnState:S = null.asInstanceOf[S]
  private var cs:CommandStream[S, StateCommand[S]] = null

  def handle(state:S, action: A):S = {
    cs = translator.translateToCommandStream(action)
    returnState = state
    loopThroughCommandStream()
    returnState
  }

  private def loopThroughCommandStream() {
     var nextStep = cs.get(returnState)
     while (nextStep.isDefined) {
       val lastStateStream = (returnState, nextStep.get._2)
       executeStep(nextStep.get)
       checkForInfiniteLoop(lastStateStream)
       nextStep = cs.get(returnState)
     }
   }

   private def executeStep(step: (StateCommand[S], CommandStream[S, StateCommand[S]])) {
    val (command,nextCommandStream) = step
    returnState = command.generateTransitions(returnState)(0).transition(returnState)
    cs = nextCommandStream
  }

  private def checkForInfiniteLoop(lastStateStream: (S, CommandStream[S, StateCommand[S]])) {
    val nextStep = cs.get(returnState)
    if (nextStep.isDefined && lastStateStream ==(returnState, nextStep.get._2))
      throw new InfiniteLoopException
  }
}