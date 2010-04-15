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
package vcc.dnd4e.domain.tracker.common

import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import org.specs.mock.Mockito

@RunWith(classOf[JUnitSuiteRunner])
class CombatStateRulesTest extends JUnit4(CombatStateRulesSpec)

object CombatStateRulesSpec extends Specification with Mockito {
  val cidA = CombatantID("A")
  val cidB = CombatantID("B")
  val cidC = CombatantID("C")
  val ioA0 = InitiativeOrderID(cidA, 0)
  val ioA1 = InitiativeOrderID(cidA, 1)
  val ioB0 = InitiativeOrderID(cidB, 0)


  val rules = new CombatStateRules
  var state: CombatStateView = null
  var mockCombA: CombatantStateView = null
  var mockCombB: CombatantStateView = null

  state = mock[CombatStateView]
  mockCombA = mock[CombatantStateView]
  mockCombB = mock[CombatantStateView]

  val baseMockups = beforeContext {
    state.getInitiativeOrder returns List(ioA0, ioB0, ioA1)
    state.combatantViewFromID(cidA) returns mockCombA
    state.combatantViewFromID(cidB) returns mockCombB
  }

  "CombatStateRules.areAllCombatantInOrderDead" ->- (baseMockups) should {
    "return true if all are dead" in {
      val minion = HealthTracker.createTracker(MinionHealthDefinition())
      val deadMinion = minion.applyDamage(10)

      deadMinion.status must_== HealthTracker.Status.Dead

      mockCombA.healthTracker returns deadMinion
      mockCombB.healthTracker returns deadMinion

      rules.areAllCombatantInOrderDead(state) must beTrue
      there was one(state).getInitiativeOrder
      there was atLeastOne(mockCombA).healthTracker
      there was atLeastOne(mockCombB).healthTracker
    }

    "return false if at least one is non dead" in {
      val minion = HealthTracker.createTracker(MinionHealthDefinition())
      val deadMinion = minion.applyDamage(10)

      deadMinion.status must_== HealthTracker.Status.Dead

      mockCombA.healthTracker returns deadMinion
      mockCombB.healthTracker returns minion

      rules.areAllCombatantInOrderDead(state) must beFalse
      there was one(state).getInitiativeOrder
      there was atLeastOne(mockCombA).healthTracker
      there was atLeastOne(mockCombB).healthTracker
    }
  }
  "rules.canCombatantRollInitiative" ->- (baseMockups) should {
    "return true if combat is not started" in {
      state.isCombatStarted returns false

      rules.canCombatantRollInitiative(state, cidA) must beTrue

      there was one(state).isCombatStarted
    }

    "return false is combat started and combatant has at least one InitiativeOrderID" in {
      state.isCombatStarted returns true

      rules.canCombatantRollInitiative(state, cidA) must beFalse

      there was one(state).isCombatStarted
      there was one(state).getInitiativeOrder
    }

    "return true is combat started and combatant has no InitiativeOrderID" in {
      state.isCombatStarted returns true

      rules.canCombatantRollInitiative(state, cidC) must beTrue

      there was one(state).isCombatStarted
      there was one(state).getInitiativeOrder
    }
  }

  "rules.hasActingCombatant" should {
    "return true if at order has one combatant" in {
      state.getInitiativeOrder returns List(ioA0)
      rules.hasActingCombatant(state) must beTrue
      there was atLeastOne(state).getInitiativeOrder
    }

    "return false if there are no combatant" in {
      state.getInitiativeOrder returns Nil
      rules.hasActingCombatant(state) must beFalse
      there was one(state).getInitiativeOrder
    }
  }
}