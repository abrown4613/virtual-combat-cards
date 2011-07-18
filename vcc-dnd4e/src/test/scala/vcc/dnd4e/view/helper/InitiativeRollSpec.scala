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
package vcc.dnd4e.view.helper


import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import java.text.ParseException
import vcc.util.DiceGenerator
import org.specs.mock.Mockito

@RunWith(classOf[JUnitSuiteRunner])
class InitiativeRollTest extends JUnit4(InitiativeRollSpec)

object InitiativeRollSpec extends Specification with Mockito {
  "a InitiativeRoll.toString" should {
    "format Nil rolls to empty string" in {
      val ir = InitiativeRoll(Nil)
      ir.toString must_== ""
    }

    "format Single None to r" in {
      val ir = InitiativeRoll(List(None))
      ir.toString must_== "r"
    }

    "format  Single Some to value" in {
      val ir = InitiativeRoll(List(Some(10)))
      ir.toString must_== "10"
    }

    "format  Mixed into a string" in {
      val ir = InitiativeRoll(List(Some(10), None, Some(4)))
      ir.toString must_== "10/r/4"
    }
  }

  "a InitiativeRoll.fromString" should {
    "throw exception in on bad string" in {
      InitiativeRoll.fromString("any") must throwA[ParseException]

      InitiativeRoll.fromString("10-r") must throwA[ParseException]

      InitiativeRoll.fromString("10/a/4") must throwA[ParseException]

      InitiativeRoll.fromString("/r/4") must throwA[ParseException]
    }

    "parse single number in fluff" in {
      val ir = InitiativeRoll.fromString("     12    ")
      ir.rolls must_== List(Some(12))
    }

    "parse single r in fluff" in {
      val ir = InitiativeRoll.fromString("     r    ")
      ir.rolls must_== List(None)
    }

    "parse complex string" in {
      val ir = InitiativeRoll.fromString("3/r/4/5")
      ir.rolls must_== List(Some(3), None, Some(4), Some(5))
    }

    "parse complex string in fluff" in {
      val ir = InitiativeRoll.fromString("  3 /  R / 4  /  5  ")
      ir.rolls must_== List(Some(3), None, Some(4), Some(5))
    }

    "parse space to Nil " in {
      val ir = InitiativeRoll.fromString("     ")
      ir.rolls must_== List()
    }
  }
  "a InitiativeRoll.resolve" should {
    val db = mock[DiceGenerator]

    "return empty on an empty list" in {
      InitiativeRoll(Nil).resolve(0, db) must_== Nil
    }

    "add bonus to informed number" in {
      InitiativeRoll.fromString("10").resolve(3, db) must_== List(13)
      there was no(db).D(20)
    }

    "return the result of a dice if undefined" in {
      db.D(20) returns 13
      InitiativeRoll.simpleRoll.resolve(2, db) must_== List(15)
      there was one(db).D(20)
    }

    "apply same logic to all" in {
      db.D(20) returns 17
      InitiativeRoll.fromString("10/r/4").resolve(4, db) must_== List(14, 21, 8)
      there was one(db).D(20)
    }
  }
}
