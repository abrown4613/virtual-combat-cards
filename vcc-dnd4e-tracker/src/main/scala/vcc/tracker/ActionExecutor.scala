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

trait ActionStreamTranslator[S, A] {
  def translateToCommandStream(action: A): CommandStream[S, StateCommand[S]]
}

class ActionExecutor[S, A](translator: ActionStreamTranslator[S, A], commandDispatcher: StateCommandDispatcher[S], builder: TransitionBuilder[S, A]) {
  type CmdStream = CommandStream[S, StateCommand[S]]
  type Cmd = StateCommand[S]

  protected def loopStream(state: S, stream: CmdStream): Either[(S, CmdStream), S] = {
    val next = stream.get(state)
    next match {
      case Some((command, nextStream)) =>
        builder.startCommand(command)
        Left((commandDispatcher.dispatch(state, command, builder), nextStream))
      case None => Right(state)
    }
  }

  def executeCommand(state: S, action: A): Transaction[S, A] = {
    builder.startTransaction(state, action)
    val stream = translator.translateToCommandStream(action)
    var steps: Either[(S, CmdStream), S] = Left(state, stream)
    while (steps.isLeft) {
      steps = loopStream(steps.left.get._1, steps.left.get._2)
    }
    builder.build(state, steps.right.get)
  }
}

