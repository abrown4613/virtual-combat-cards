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

case class RulingContext[S](state: S, triggeringCommand: Command[S], rulingNeedingDecision: List[Ruling[S, _, _]])

trait RulingProvider[S] {

  def provideRulingFor(context: RulingContext[S]): List[Ruling[S, _, _]]
}

object ActionDispatcher {

  class RulingsAndDecisionsMismatchException(rulings: List[Ruling[_, _, _]], decisions: List[Ruling[_, _, _]])
    extends RuntimeException(
      "Ruling and decision mismatch: %s <-> %s".format(rulings.mkString(", "), decisions.mkString(", ")))

  class IllegalEventException(event: Event[_], causingException: Throwable)
    extends RuntimeException("Event could not be processed: " + event, causingException)

  class IllegalCommandException(command: Command[_], causingException: Throwable)
    extends RuntimeException("Event could not be processed: " + command, causingException)

  def getDispatcher[S](state: S): ActionDispatcher[S] = new ActionDispatcher[S](state)
}

class ActionDispatcher[S] private(initialState: S) {

  private var finalState: Option[S] = None
  private var currentState: S = initialState
  private var commandStream: CommandStream[S] = null
  private var rulingProvider: RulingProvider[S] = null

  def resultState: Option[S] = finalState

  def setRulingProvider(rulingProvider: RulingProvider[S]) {
    this.rulingProvider = rulingProvider
  }

  def handle(action: Action[S]) {
    checkForRepeatedDispatch()

    commandStream = action.createCommandStream()
    loopThroughCommandStream()
    finalState = Some(currentState)
  }

  private def checkForRepeatedDispatch() {
    if (commandStream != null)
      throw new IllegalStateException("Cant do second dispatch")
  }

  private def loopThroughCommandStream() {
    var nextStep = commandStream.get(currentState)
    while (nextStep.isDefined) {
      processStep(nextStep.get._1, nextStep.get._2)
      nextStep = commandStream.get(currentState)
    }
  }

  private def processStep(command: Command[S], nextStream: CommandStream[S]) {
    val previousState = currentState
    executeCommand(command)
    checkForInfiniteLoop(previousState, commandStream)
    commandStream = nextStream
  }

  private def executeCommand(command: Command[S]) {
    val rulingCommands = collectRulingCommands(command)

    rulingCommands.foreach(executeCommand)
    processCommandEvents(command)
  }

  private def collectRulingCommands(command: Command[S]): List[Command[S]] = {
    val requiredRulings = command.requiredRulings(currentState)
    if (requiredRulings.isEmpty)
      Nil
    else
      collectDecisionsForRulings(command, requiredRulings)
  }

  private def collectDecisionsForRulings(command: Command[S], requiredRulings: List[Ruling[S, _, _]]): List[Command[S]] = {
    val context = RulingContext(currentState, command, requiredRulings)
    val decisions = rulingProvider.provideRulingFor(context)
    validateDecisions(requiredRulings, decisions)
    decisions.flatMap(r => r.generateCommands(currentState))
  }

  private def validateDecisions(rulings: List[Ruling[S, _, _]], decisions: List[Ruling[S, _, _]]) {
    if (rulings.length != decisions.length)
      throw new ActionDispatcher.RulingsAndDecisionsMismatchException(rulings, decisions)

    for ((ruling, decision) <- rulings zip decisions)
      if (!ruling.isRulingSameSubject(decision))
        throw new ActionDispatcher.RulingsAndDecisionsMismatchException(rulings, decisions)
  }

  private def checkForInfiniteLoop(previousState: S, previousCommandStream: CommandStream[S]) {
    if (previousState == currentState && previousCommandStream == nextStreamWithCurrentStateOrNull)
      throw new InfiniteLoopException

    def nextStreamWithCurrentStateOrNull: CommandStream[S] = {
      commandStream.get(currentState).map(_._2).getOrElse(null)
    }
  }

  def processCommandEvents(command: Command[S]) {
    val events = generateEventOrReportException(command)
    for (event <- events) {
      processEventOrReportException(event)
    }

    def processEventOrReportException(event: Event[S]) {
      try {
        currentState = event.transition(currentState)
      } catch {
        case e => throw new ActionDispatcher.IllegalEventException(event, e)
      }
    }
  }

  private def generateEventOrReportException(command: Command[S]): List[Event[S]] = {
    try {
      command.generateEvents(currentState)
    } catch {
      case e => throw new ActionDispatcher.IllegalCommandException(command, e)
    }
  }
}