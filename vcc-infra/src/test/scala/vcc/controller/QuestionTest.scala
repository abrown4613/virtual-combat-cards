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
package vcc.controller

import org.specs2.mutable.SpecificationWithJUnit

class QuestionTest extends SpecificationWithJUnit {
  val q1 = YesNoRuling("Is it pink?")
  val q2 = YesNoRuling("Is it magenta?")
  val answer = YesNoDecision(q1, true)

  "YesNoQuestion" should {

    "accept answer to the correct question" in {
      q1.isValidDecision(answer) must beTrue
    }
    "accept answer to the correct question" in {
      q2.isValidDecision(answer) must beFalse
    }
  }

  "Pending Answer" should {

    val pending: PendingRuling[_] = new PendingRuling[Boolean](q1, answer => {
      answer match {
        case YesNoDecision(q, a) => a
        case _ => throw new IllegalArgumentException("Should not be here")
      }
    })

    "provide Some when properly answered" in {
      pending.processDecision(answer) must_== Some(true)
    }

    "provide None is the answer is incorrect" in {
      pending.processDecision(YesNoDecision(q2, false)) must_== None
    }
  }
}