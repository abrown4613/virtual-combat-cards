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

trait RulingProvider[S] {
  def provideRulingFor(rulingNeedingDecision: List[Ruling[S, _, _]]): List[Ruling[S, _, _]]
}

class ActionDispatcher[S, A](translator: ActionStreamTranslator[S, A], rulingLocator: RulingLocationService[S]) {

  private var rulingProvider:RulingProvider[S] = null

  def setRulingProvider(rulingProvider: RulingProvider[S]) {
    this.rulingProvider = rulingProvider
  }

  private var returnState: S = null.asInstanceOf[S]
  private var commandStream: CommandStream[S, StateCommand[S]] = null

  def handle(state: S, action: A): S = {
    commandStream = translator.translateToCommandStream(action)
    returnState = state
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

  def processCommandEvents(command: StateCommand[S]) {
    val transitions = command.generateTransitions(returnState)
    returnState = transitions.foldLeft(returnState)((s, t) => t.transition(s))
  }

  private def executeStep(step: (StateCommand[S], CommandStream[S, StateCommand[S]])) {
    val (command, nextCommandStream) = step
    val rulings = rulingLocator.rulingsFromStateWithCommand(returnState, command)
    if (!rulings.isEmpty) {
      val decisions = rulingProvider.provideRulingFor(rulings)
      val rulingCommands = decisions.flatMap(r => r.generateCommands(returnState))
      for( rulingCommand <- rulingCommands) {
        returnState = rulingCommand.generateTransitions(returnState)(0).transition(returnState)
      }
    }
    processCommandEvents(command)
    commandStream = nextCommandStream
  }

  private def checkForInfiniteLoop(lastStateStream: (S, CommandStream[S, StateCommand[S]])) {
    val nextStep = commandStream.get(returnState)
    if (nextStep.isDefined && lastStateStream ==(returnState, nextStep.get._2))
      throw new InfiniteLoopException
  }
}