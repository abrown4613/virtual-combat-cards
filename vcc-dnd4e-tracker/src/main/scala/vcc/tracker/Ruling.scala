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

/**
 * What is being decided by the Ruling
 */
trait Question[S] {
  def userPrompt(state: S): String
}

class IllegalAnswerException(msg: String) extends Exception(msg)

/**
 * A request for a ruling or a completed ruling.
 */
abstract class Ruling[S, Q <: Question[S], A] {
  type R <: Ruling[S, Q, A]
  val question: Q
  val answer: Option[A]

  def hasDecision: Boolean = answer.isDefined

  protected def commandsFromAnswer(state: S): List[StateCommand[S]]

  def generateCommands(state: S): List[StateCommand[S]] = {
    if (hasDecision) commandsFromAnswer(state)
    else throw new IllegalAnswerException("No answer for ruling " + question.userPrompt(state))
  }

  def withAnswer(value: A): R
}


