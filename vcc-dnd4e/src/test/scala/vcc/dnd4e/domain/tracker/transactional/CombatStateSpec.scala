/**
 * Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.transactional


import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import org.specs.mock.Mockito
import vcc.dnd4e.domain.tracker.common.{InitiativeTracker, InitiativeOrderID, CombatantID}

@RunWith(classOf[JUnitSuiteRunner])
class CombatStateTest extends JUnit4(CombatStateSpec)

object CombatStateSpec extends Specification with Mockito {
  // Useful objects
  val combA = CombatantID("A")
  val ioa = InitiativeOrderID(combA, 3)

  // Mocks and testee
  val mRoster = mock[CombatantRoster]
  val mOrder = mock[InitiativeOrder]
  val mMeta = mock[CombatMetaData]
  val aCombatState = new CombatState(mOrder, mRoster, mMeta)

  "aCombatState" should {
    "ask CombatMetaData for inCombat" in {
      mMeta.inCombat returns true
      aCombatState.inCombat must beTrue
      there was one(mMeta).inCombat
    }

    "lift a CombatantID to a Combatant when present" in {
      mRoster.isDefinedAt(combA) returns true
      mRoster.combatant(combA) returns mock[Combatant]

      val comb: Combatant = combA match {
        case aCombatState.combatantFromID(comb) => comb
        case _ => null
      }

      comb must notBeNull
      comb.isInstanceOf[Combatant] must beTrue
      there was one(mRoster).isDefinedAt(combA) then
              one(mRoster).combatant(combA)
    }

    "match CombatantID to None when not present" in {
      mRoster.isDefinedAt(combA) returns false

      val comb: Combatant = combA match {
        case aCombatState.combatantFromID(comb) => comb
        case _ => null
      }

      comb must beNull
      there was one(mRoster).isDefinedAt(combA)
      there was no(mRoster).combatant(combA)
    }


    "lift a InitiativeOrderID to a InitiativeTracker when present" in {
      mOrder.isDefinedAt(ioa) returns true
      mOrder.initiativeTrackerFor(ioa) returns mock[InitiativeTracker]

      val it: InitiativeTracker = ioa match {
        case aCombatState.initTrackerFromID(tracker) => tracker
        case _ => null
      }

      it must notBeNull
      it.isInstanceOf[InitiativeTracker] must beTrue
      there was one(mOrder).isDefinedAt(ioa) then
              one(mOrder).initiativeTrackerFor(ioa)
    }

    "match InitiativeOrderID to None when an InitiativeTracker is not present" in {
      mOrder.isDefinedAt(ioa) returns false

      val it: InitiativeTracker = ioa match {
        case aCombatState.initTrackerFromID(tracker) => tracker
        case _ => null
      }

      it must beNull
      there was one(mOrder).isDefinedAt(ioa)
      there was no(mOrder).initiativeTrackerFor(ioa)
    }
  }
}