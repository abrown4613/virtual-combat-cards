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
package vcc.dnd4e.domain.tracker.snapshot


import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import vcc.dnd4e.domain.tracker.common.CombatantStateView
import org.specs.mock.Mockito

@RunWith(classOf[JUnitSuiteRunner])
class CombatStateTest extends JUnit4(CombatStateSpec)

object CombatStateSpec extends Specification with Mockito with CombatStateSnapshotHelper[String] {
  "a CombatState" should {
    "provide the correct list of combatantsNotInOrder" in {
      val cs = CombatState(
        false,
        null,
        List(ioa0), Map(ioa0 -> ita0),
        Some(ioa0),
        Map(combA -> mock[CombatantStateView], combB -> mock[CombatantStateView], combC -> mock[CombatantStateView]))
      cs.combatantsNotInOrder() must_== Set(combB, combC)
    }
  }

}