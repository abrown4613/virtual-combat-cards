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
package vcc.dnd4e.tracker.ruling

import org.specs2.SpecificationWithJUnit
import org.specs2.specification.Fragments
import vcc.tracker.{UndecidedRulingException, Command, Ruling}

abstract class RulingAcceptance[S](testTitle: String) extends SpecificationWithJUnit {

  private case class DummyRuling(decision: Option[Int]) extends Ruling[S, Int, DummyRuling] {
    def withDecision(decision: Int): DummyRuling = copy(decision = Some(decision))

    def isRulingSameSubject(otherRuling: Ruling[S, _, _]): Boolean = false

    def userPrompt(state: S): String = null

    protected def commandsFromDecision(state: S): List[Command[S]] = Nil
  }

  protected val rulingWithAnswer: Ruling[S, _, _]
  protected val rulingWithoutAnswer: Ruling[S, _, _]
  protected val userPromptMessage: String
  protected val state: S

  def is = testTitle.title ^
    baseCases ^
    "build cases" ^ buildCases ^ endp ^
    end

  private def baseCases = {
    "base cases" ^
    "  have correct prompt" ! haveCorrectPrompt ^
    "  match answered and unanswered" ! answeredMatchUnanswered ^
    "  not match some other ruling" ! notMatchRuling ^
    "  have answer when it has" ! answeredHasDecision ^
    "  not have answer when it doesn't" ! unansweredHasNoDecision ^
    "  throw exception on commands from Ruling without decision" ! unansweredCantGenerateCommand ^
    endp
  }

  private def haveCorrectPrompt = {
    rulingWithAnswer.userPrompt(state) must_== userPromptMessage
  }

  private def answeredMatchUnanswered = {
    rulingWithAnswer.isRulingSameSubject(rulingWithoutAnswer) must beTrue
  }

  private def notMatchRuling = {
    rulingWithAnswer.isRulingSameSubject(DummyRuling(None)) must beFalse
  }

  private def unansweredHasNoDecision = {
    rulingWithoutAnswer.hasDecision must beFalse
  }

  private def unansweredCantGenerateCommand = {
    rulingWithoutAnswer.generateCommands(state) must throwA[UndecidedRulingException]
  }

  private def answeredHasDecision = {
    rulingWithAnswer.hasDecision must beTrue
  }

  def buildCases: Fragments
}