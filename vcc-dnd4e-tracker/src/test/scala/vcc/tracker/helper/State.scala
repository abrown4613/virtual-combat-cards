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
package vcc.tracker.helper

import vcc.tracker._
import java.lang.IllegalStateException

case class State(value: Int)

case class LoopToAction(limit: Int, step: Int) extends Action[State] {
  def createCommandStream(): CommandStream[State] = {
    new PartialFunctionCommandStream[State]({
      case State(current) if (current < limit) => FlexCommand(IncrementEvent(step))
    })
  }
}

case class FlexAction(commands: Command[State]*) extends Action[State] {
  def createCommandStream(): CommandStream[State] = CommandStream(commands: _*)
}

case class FlexCommand(rulingPrompts: List[String], events: List[Event[State]]) extends Command[State] {
  def generateEvents(state: State): List[Event[State]] = events

  override def requiredRulings(state: State): List[Ruling[State, _, _]] = {
    rulingPrompts.map(FlexRuling(_, None))
  }
}

object FlexCommand {
  def apply(events: Event[State]*) = new FlexCommand(Nil, events.toList)

  def apply(rulingPrompt: String, events: Event[State]*) = new FlexCommand(rulingPrompt::Nil, events.toList)
}

case class CrashCommand(identifier:String) extends Command[State] {
  override def generateEvents(state: State): List[Event[State]] = throw new IllegalStateException(identifier)
}

case class SetStateEvent(value: Int) extends Event[State] {
  def transition(iState: State): State = State(value)
}

case class IncrementEvent(inc: Int) extends Event[State] {
  def transition(iState: State): State = State(iState.value + inc)
}

case class CrashEvent(identifier: String) extends Event[State] {
  def transition(iState: State): State = throw new IllegalStateException(identifier)
}

case class FlexRuling(prompt: String, decision: Option[List[Command[State]]])
  extends Ruling[State, List[Command[State]], FlexRuling] {

  def isRulingSameSubject(otherRuling: Ruling[State, _, _]): Boolean = {
    otherRuling.asInstanceOf[AnyRef] match {
      case FlexRuling(`prompt`, _) => true
      case _ => false
    }
  }

  def userPrompt(state: State): String = "%s [%d]".format(prompt, state.value)

  protected def commandsFromDecision(state: State): List[Command[State]] = decision.get

  def withDecision(decision: List[Command[State]]): FlexRuling = null
}