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
package vcc.dnd4e.view.dialog

import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import vcc.dnd4e.tracker.common.CombatantID
import vcc.dnd4e.view.helper.InitiativeRoll

@RunWith(classOf[JUnitSuiteRunner])
class InitiativeDialogTest extends JUnit4(InitiativeDialogSpec)

object InitiativeDialogSpec extends Specification {
  val combA = CombatantID("A")
  val combB = CombatantID("B")
  val combC = CombatantID("C")

  "InitiativeDialogEntry" should {
    "be similar is name, roll and bonus are the same" in {
      val iea = new InitiativeDialogEntry(Set(combA), "name", 4, InitiativeRoll.simpleRoll, false)
      val ieb = new InitiativeDialogEntry(Set(combB), "name", 4, InitiativeRoll.simpleRoll, false)

      (iea isSimilar ieb) must beTrue
    }

    "be diferente otherwise" in {
      val iea = new InitiativeDialogEntry(Set(combA), "name", 4, InitiativeRoll.simpleRoll, false)
      val ieb = new InitiativeDialogEntry(Set(combB), "name2", 4, InitiativeRoll.simpleRoll, false)

      (iea isSimilar ieb) must beFalse
    }

    "be diferente otherwise" in {
      val iea = new InitiativeDialogEntry(Set(combA), "name", 4, InitiativeRoll.simpleRoll, false)
      val ieb = new InitiativeDialogEntry(Set(combB), "name", 5, InitiativeRoll.simpleRoll, false)

      (iea isSimilar ieb) must beFalse
    }

    "be diferente otherwise" in {
      val iea = new InitiativeDialogEntry(Set(combA), "name", 4, InitiativeRoll.simpleRoll, false)
      val ieb = new InitiativeDialogEntry(Set(combB), "name", 4, InitiativeRoll(List(Some(4))), false)

      (iea isSimilar ieb) must beFalse
    }
  }

}