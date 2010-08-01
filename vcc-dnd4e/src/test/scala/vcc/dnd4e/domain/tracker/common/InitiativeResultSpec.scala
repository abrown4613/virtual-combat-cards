/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
//$Id$
package vcc.dnd4e.domain.tracker.common

import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}

@RunWith(classOf[JUnitSuiteRunner])
class InitiativeResultTest extends JUnit4(InitiativeResultSpec)

object InitiativeResultSpec extends Specification {
  "InitiativeResult ordering " should {
    "make larger bonus + init greater" in {
      (InitiativeResult(null, 1, 10, 0) < InitiativeResult(null, 1, 11, 0)) must beTrue
    }

    "make larger bonus + init greater" in {
      (InitiativeResult(null, 1, 11, 0) > InitiativeResult(null, 1, 10, 0)) must beTrue
    }

    "make larger bonus + init less " in {
      (InitiativeResult(null, 1, 10, 0) > InitiativeResult(null, 1, 9, 0)) must beTrue
    }

    "make larger bonus + init less " in {
      (InitiativeResult(null, 1, 9, 0) < InitiativeResult(null, 1, 10, 0)) must beTrue
    }

    "if same bonus and init" in {
      (InitiativeResult(null, 1, 10, 0) compareTo InitiativeResult(null, 1, 10, 0)) must_== 0
    }

    "return larger bonus if (bonus+init) match" in {
      (InitiativeResult(null, 2, 10, 0) > InitiativeResult(null, 1, 10, 0)) must beTrue
    }

    "return largest of the tie breaks if both are set  and item match" in {
      val a = InitiativeResult(null, 1, 10, 1)
      val b = InitiativeResult(null, 1, 10, 2)

      a.compare(b) must_!= 0
      a < b must beTrue
    }

    "same base result with only one tiebreaker must yeild match" in {
      val a = InitiativeResult(null, 1, 10, 1)
      val b = InitiativeResult(null, 1, 10, 0)
      a.compare(b) must_== 0
    }
  }
}