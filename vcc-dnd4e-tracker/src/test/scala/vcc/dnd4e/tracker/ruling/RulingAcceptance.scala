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
import vcc.dnd4e.tracker.common.CombatState
import org.specs2.specification.Fragments
import vcc.tracker.{StateCommand, Ruling}

abstract class RulingAcceptance(testTitle: String) extends SpecificationWithJUnit {

  private case class DummyRuling(decision: Option[Int]) extends Ruling[CombatState, Int, DummyRuling] {
    def withDecision(decision: Int): DummyRuling = copy(decision = Some(decision))

    def isRulingSameSubject(otherRuling: Ruling[CombatState, _, _]): Boolean = false

    def userPrompt(state: CombatState): String = null

    protected def commandsFromDecision(state: CombatState): List[StateCommand[CombatState]] = Nil
  }

  protected val rulingWithAnswer: Ruling[CombatState, _, _]
  protected val rulingWithoutAnswer: Ruling[CombatState, _, _]
  protected val userPromptMessage: String
  protected val state: CombatState

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

  private def answeredHasDecision = {
    rulingWithAnswer.hasDecision must beTrue
  }

  def buildCases: Fragments
}