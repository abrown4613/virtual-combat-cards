/*
 *  Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.common

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import vcc.dnd4e.tracker.common._
import org.specs2.specification.Scope

class CombatStateRulesTest extends SpecificationWithJUnit with Mockito {
  private val cidA = CombatantID("A")
  private val cidB = CombatantID("B")
  private val cidC = CombatantID("C")
  private val ioA0 = InitiativeOrderID(cidA, 0)
  private val ioA1 = InitiativeOrderID(cidA, 1)
  private val ioB0 = InitiativeOrderID(cidB, 0)

  val rules = new CombatStateRules

  trait baseMockups extends Scope {
    val state: CombatStateView = mock[CombatStateView]
    val mockCombA = mock[CombatantStateView]
    val mockCombB = mock[CombatantStateView]

    state.getInitiativeOrder returns List(ioA0, ioB0, ioA1)
    state.combatantViewFromID(cidA) returns mockCombA
    state.combatantViewFromID(cidB) returns mockCombB
  }

  "CombatStateRules.areAllCombatantInOrderDead" should {
    "return true if all are dead" in new baseMockups {
      val minion = HealthTracker.createTracker(MinionHealthDefinition)
      val deadMinion = minion.applyDamage(10)

      deadMinion.status must_== HealthStatus.Dead

      mockCombA.health returns deadMinion
      mockCombB.health returns deadMinion

      rules.areAllCombatantInOrderDead(state) must beTrue
      there was one(state).getInitiativeOrder
      there was atLeastOne(mockCombA).health
      there was atLeastOne(mockCombB).health
    }

    "return false if at least one is non dead" in new baseMockups {
      val minion = HealthTracker.createTracker(MinionHealthDefinition)
      val deadMinion = minion.applyDamage(10)

      deadMinion.status must_== HealthStatus.Dead

      mockCombA.health returns deadMinion
      mockCombB.health returns minion

      rules.areAllCombatantInOrderDead(state) must beFalse
      there was one(state).getInitiativeOrder
      there was atLeastOne(mockCombA).health
      there was atLeastOne(mockCombB).health
    }
  }

  "rules.canCombatantRollInitiative" should {
    "return true if combat is not started" in new baseMockups {
      state.isCombatStarted returns false

      rules.canCombatantRollInitiative(state, cidA) must beTrue

      there was one(state).isCombatStarted
    }

    "return false is combat started and combatant has at least one InitiativeOrderID" in new baseMockups {
      state.isCombatStarted returns true

      rules.canCombatantRollInitiative(state, cidA) must beFalse

      there was one(state).isCombatStarted
      there was one(state).getInitiativeOrder
    }

    "return true is combat started and combatant has no InitiativeOrderID" in new baseMockups {
      state.isCombatStarted returns true

      rules.canCombatantRollInitiative(state, cidC) must beTrue

      there was one(state).isCombatStarted
      there was one(state).getInitiativeOrder
    }
  }

  "rules.hasActingCombatant" should {
    "return true if at order has one combatant" in new baseMockups {
      state.getInitiativeOrder returns List(ioA0)
      rules.hasActingCombatant(state) must beTrue
      there was atLeastOne(state).getInitiativeOrder
    }

    "return false if there are no combatant" in new baseMockups {
      state.getInitiativeOrder returns Nil
      rules.hasActingCombatant(state) must beFalse
      there was one(state).getInitiativeOrder
    }
  }

  "rules.canMoveBefore" should {

    "not allow move if not in combat" in new baseMockups {
      state.isCombatStarted returns false
      state.initiativeTrackerFromID(ioA0) returns InitiativeTracker.initialTracker(ioA0, 0)
      rules.canMoveBefore(state, ioA0, ioB0) must beFalse
      there was one(state).isCombatStarted
    }

    "not allow acting to move" in new baseMockups {
      state.isCombatStarted returns true
      state.initiativeTrackerFromID(ioA0) returns InitiativeTracker(ioA0, 0, 0, InitiativeState.Acting)
      rules.canMoveBefore(state, ioA0, ioB0) must beFalse
      there was one(state).initiativeTrackerFromID(ioA0)
    }

    "not allow move of self before self" in new baseMockups {
      rules.canMoveBefore(state, ioA0, ioA0) must beFalse
    }

    "allow otherwise" in new baseMockups {
      state.isCombatStarted returns true
      state.initiativeTrackerFromID(ioA0) returns InitiativeTracker(ioA0, 0, 0, InitiativeState.Waiting)
      rules.canMoveBefore(state, ioA0, ioB0) must beTrue
      there was one(state).initiativeTrackerFromID(ioA0)

      state.initiativeTrackerFromID(ioA0) returns InitiativeTracker(ioA0, 0, 0, InitiativeState.Delaying)
      rules.canMoveBefore(state, ioA0, ioB0) must beTrue

      state.initiativeTrackerFromID(ioA0) returns InitiativeTracker(ioA0, 0, 0, InitiativeState.Ready)
      rules.canMoveBefore(state, ioA0, ioB0) must beTrue

    }
  }

  "rules.canInitiativeOrderPerform" should {

    // Should only be able to take actions during combat
    // The first for these actions is not element 0 of the other but whomever is the nextUp

    "work only when in combat" in new baseMockups {
      state.isCombatStarted returns false
      state.nextUp returns None
      rules.canInitiativeOrderPerform(state, ioA0, InitiativeAction.StartRound) must beFalse
      there was one(state).isCombatStarted
      there was no(state).getInitiativeOrder
    }

    "only allow initiative actions during combat" in new baseMockups {
      state.isCombatStarted returns true
      state.nextUp returns Some(ioA1)
      state.initiativeTrackerFromID(ioA0) returns mock[InitiativeTracker]
      state.initiativeTrackerFromID(ioA1) returns mock[InitiativeTracker]

      rules.canInitiativeOrderPerform(state, ioA0, InitiativeAction.StartRound) must beFalse
      there was one(state).isCombatStarted andThen
        atLeastOne(state).nextUp
    }

    "defer to InitiativeTracker with the correct first InitiativeTracker set" in new baseMockups {
      val mIT = mock[InitiativeTracker]
      val fIT = mock[InitiativeTracker]
      val action = InitiativeAction.StartRound

      mIT.canTransform(fIT, action) returns false

      state.isCombatStarted returns true
      state.nextUp returns Some(ioA1)
      state.initiativeTrackerFromID(ioB0) returns mIT
      state.initiativeTrackerFromID(ioA1) returns fIT

      rules.canInitiativeOrderPerform(state, ioB0, action) must beFalse
      there was one(state).initiativeTrackerFromID(ioA1)
      there was one(state).initiativeTrackerFromID(ioB0)
      there was one(mIT).canTransform(fIT, action)
    }
  }

  "rules.areAllied" should {
    "be true if combatant are both character" in new baseMockups {
      mockCombA.combatantType returns CombatantType.Character
      mockCombB.combatantType returns CombatantType.Character
      rules.areAllied(state, cidA, cidB) must beTrue
      there was one(mockCombA).combatantType
      there was one(mockCombB).combatantType
    }

    "be true if combatant are both are not character" in new baseMockups {
      mockCombA.combatantType returns CombatantType.Monster
      mockCombB.combatantType returns CombatantType.Minion
      rules.areAllied(state, cidA, cidB) must beTrue

      mockCombA.combatantType returns CombatantType.Monster
      mockCombB.combatantType returns CombatantType.Minion
      rules.areAllied(state, cidA, cidB) must beTrue

      mockCombA.combatantType returns CombatantType.Minion
      mockCombB.combatantType returns CombatantType.Minion
      rules.areAllied(state, cidA, cidB) must beTrue
    }

    "be false otherwise" in new baseMockups {
      mockCombA.combatantType returns CombatantType.Character
      mockCombB.combatantType returns CombatantType.Minion
      rules.areAllied(state, cidA, cidB) must beFalse

      mockCombB.combatantType returns CombatantType.Monster
      rules.areAllied(state, cidA, cidB) must beFalse
    }
  }
}