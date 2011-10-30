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
  def handle(state:S, action: A):S = {
    var cs = translator.translateToCommandStream(action)
    var nextStep = cs.get(state)
    var returnState = state
    while(nextStep.isDefined) {
      val (command,nextCommandStream) = nextStep.get
      val lastStateStream = (returnState, nextCommandStream)
      returnState = command.generateTransitions(returnState)(0).transition(returnState)
      cs = nextCommandStream
      nextStep = cs.get(returnState)
      if(nextStep.isDefined && lastStateStream == (returnState, nextStep.get._2))
        throw new InfiniteLoopException
    }
    returnState
  }
}