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

import java.lang.IllegalStateException

class InfiniteLoopException extends RuntimeException

trait RulingProvider[S] {
  def provideRulingFor(state: S, rulingNeedingDecision: List[Ruling[S, _, _]]): List[Ruling[S, _, _]]
}

object ActionDispatcher {
  def getDispatcher[S](state: S): ActionDispatcher[S] = new ActionDispatcher[S](state)
}

class ActionDispatcher[S] private(initialState: S) {

  def resultState: Option[S] = Some(returnState)

  private var rulingProvider: RulingProvider[S] = null

  def setRulingProvider(rulingProvider: RulingProvider[S]) {
    this.rulingProvider = rulingProvider
  }

  private var returnState: S = initialState
  private var commandStream: CommandStream[S] = null

  def handle(action: Action[S]) {
    if (commandStream != null)
      throw new IllegalStateException("Cant do second dispatch")

    commandStream = action.createCommandStream()
    loopThroughCommandStream()
    returnState
  }

  private def loopThroughCommandStream() {
    var nextStep = commandStream.get(returnState)
    while (nextStep.isDefined) {
      val lastStateStream = (returnState, nextStep.get._2)
      executeStep(nextStep.get)
      checkForInfiniteLoop(lastStateStream)
      nextStep = commandStream.get(returnState)
    }
  }

  private def executeStep(step: (Command[S], CommandStream[S])) {
    val (command, nextCommandStream) = step
    val rulings = command.requiredRulings(returnState)
    if (!rulings.isEmpty) {
      val decisions = rulingProvider.provideRulingFor(returnState, rulings)
      val rulingCommands = decisions.flatMap(r => r.generateCommands(returnState))
      for (rulingCommand <- rulingCommands) {
        processCommandEvents(rulingCommand)
      }
    }
    processCommandEvents(command)
    commandStream = nextCommandStream
  }

  def processCommandEvents(command: Command[S]) {
    val transitions = command.generateEvents(returnState)
    returnState = transitions.foldLeft(returnState)((s, t) => t.transition(s))
  }

  private def checkForInfiniteLoop(lastStateStream: (S, CommandStream[S])) {
    val nextStep = commandStream.get(returnState)
    if (nextStep.isDefined && lastStateStream ==(returnState, nextStep.get._2))
      throw new InfiniteLoopException
  }
}