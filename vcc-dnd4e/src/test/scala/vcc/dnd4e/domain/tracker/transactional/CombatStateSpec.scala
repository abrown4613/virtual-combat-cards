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
package vcc.dnd4e.domain.tracker.transactional


import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import org.specs.mock.Mockito
import vcc.dnd4e.tracker.common._

@RunWith(classOf[JUnitSuiteRunner])
class CombatStateTest extends JUnit4(CombatStateSpec)

object CombatStateSpec extends Specification with Mockito {
  // Useful objects
  val combA = CombatantID("A")
  val ioa = InitiativeOrderID(combA, 3)
  val eia = EffectID(combA, 3)

  // Mocks and testee
  val mRoster = mock[CombatantRoster]
  val mOrder = mock[InitiativeOrder]
  val mMeta = mock[CombatMetaData]
  val aCombatState = new CombatState(mOrder, mRoster, mMeta)

  "aCombatState" should {
    "ask CombatMetaData for inCombat" in {
      mMeta.inCombat returns true
      aCombatState.isCombatStarted must beTrue
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

    "lift a EffectID to a Combatant when present" in {
      mRoster.isDefinedAt(combA) returns true
      mRoster.combatant(combA) returns mock[Combatant]

      val comb: Combatant = eia match {
        case aCombatState.combatantFromEffectID(comb) => comb
        case _ => null
      }

      comb must notBeNull
      comb.isInstanceOf[Combatant] must beTrue
      there was one(mRoster).isDefinedAt(combA) then
        one(mRoster).combatant(combA)
    }

    "match EffectID to None when not present" in {
      mRoster.isDefinedAt(combA) returns false

      val comb: Combatant = eia match {
        case aCombatState.combatantFromEffectID(comb) => comb
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

    "return None on a nextUp if order has not robinHead" in {
      mOrder.robinHeadInitiativeTracker returns null
      aCombatState.nextUp must_== None
    }

    "return Some on a nextUp if order has robinHead" in {
      val it = mock[InitiativeTracker]
      it.orderID returns ioa
      mOrder.robinHeadInitiativeTracker returns it
      aCombatState.nextUp must_== Some(ioa)
    }
  }

  "aCombatState as a CombatStateView" ->- (beforeContext {
    val ioA = InitiativeOrderID(CombatantID("A"), 0)
    val ioB = InitiativeOrderID(CombatantID("B"), 0)
    mRoster.allCombatantIDs returns List(combA, CombatantID("B"))
    mOrder.getIDsInOrder returns List(ioA, ioB)
    mOrder.initiativeTrackerFor(ioA) returns InitiativeTracker.initialTracker(ioA, 0)
    mOrder.initiativeTrackerFor(ioB) returns InitiativeTracker.initialTracker(ioB, 0)
    mMeta.comment returns "Fiat lux!"
    mMeta.inCombat returns true
    val csvA = mock[Combatant]
    csvA.definition returns CombatantRosterDefinition(combA, null, null)
    val csvB = mock[Combatant]
    csvB.definition returns CombatantRosterDefinition(CombatantID("B"), null, null)
    mRoster.combatant(combA) returns csvA
    mRoster.combatant(CombatantID("B")) returns csvB
  }) should {
    val cidA = CombatantID("A")
    val cidB = CombatantID("B")
    val ioa = InitiativeOrderID(CombatantID("A"), 0)
    val iob = InitiativeOrderID(CombatantID("B"), 0)

    "gets all CombatanttateView from all defined ID" in {
      for (id <- List(cidA, cidB)) {
        val cv = aCombatState.combatantViewFromID(id)
        cv must notBeNull
        cv.definition.cid must_== id
      }
    }

    "returns all defined CombatantID allCombatantIDs (not in order)" in {
      aCombatState.allCombatantIDs must contain(cidA)
      aCombatState.allCombatantIDs must contain(cidB)
    }

    "returns tracker for each ID with initiativeTrackerFromID" in {
      aCombatState.initiativeTrackerFromID(ioa) must_== InitiativeTracker.initialTracker(ioa, 0)
      aCombatState.initiativeTrackerFromID(iob) must_== InitiativeTracker.initialTracker(iob, 0)
    }

    "return a list of InitiativeOrderID on a getInitiativeOrder" in {
      aCombatState.getInitiativeOrder must_== List(ioa, iob)
    }

    "isCombatStarted provides valid answer" in {
      aCombatState.isCombatStarted must beTrue
    }

    "return comment on combatComment" in {
      aCombatState.combatComment must_== "Fiat lux!"
    }

    "be internally consistent" in {
      //All InitiativeOrderID should point to a valid CombatantStateView
      val ids = aCombatState.allCombatantIDs
      for (ioi <- aCombatState.getInitiativeOrder) {
        ids must contain(ioi.combId)
        val cv = aCombatState.combatantViewFromID(ioi.combId)
        cv must notBeNull
        cv.definition.cid must_== ioi.combId
      }
    }
  }
}