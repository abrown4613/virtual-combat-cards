/**
 *  Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
import vcc.dnd4e.model.CombatantEntity
import vcc.dnd4e.model.common.CombatantType

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

  val baseMockups = beforeContext {
    state = mock[CombatStateView]
    mockCombA = mock[CombatantStateView]
    mockCombB = mock[CombatantStateView]

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

  "rules.hasActingCombatant" ->- (baseMockups) should {
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

  "rules.canMoveBefore" ->- (baseMockups) should {

    "not allow move if not in combat" in {
      state.isCombatStarted returns false
      state.initiativeTrackerFromID(ioA0) returns InitiativeTracker.initialTracker(ioA0, 0)
      rules.canMoveBefore(state, ioA0, ioB0) must beFalse
      there was one(state).isCombatStarted
    }

    "not allow acting to move" in {
      state.isCombatStarted returns true
      state.initiativeTrackerFromID(ioA0) returns InitiativeTracker(ioA0, 0, 0, InitiativeTracker.state.Acting)
      rules.canMoveBefore(state, ioA0, ioB0) must beFalse
      there was one(state).initiativeTrackerFromID(ioA0)
    }

    "not allow move of self before self" in {
      rules.canMoveBefore(state, ioA0, ioA0) must beFalse
    }

    "allow otherwise" in {
      state.isCombatStarted returns true
      state.initiativeTrackerFromID(ioA0) returns InitiativeTracker(ioA0, 0, 0, InitiativeTracker.state.Waiting)
      rules.canMoveBefore(state, ioA0, ioB0) must beTrue
      there was one(state).initiativeTrackerFromID(ioA0)

      state.initiativeTrackerFromID(ioA0) returns InitiativeTracker(ioA0, 0, 0, InitiativeTracker.state.Delaying)
      rules.canMoveBefore(state, ioA0, ioB0) must beTrue

      state.initiativeTrackerFromID(ioA0) returns InitiativeTracker(ioA0, 0, 0, InitiativeTracker.state.Ready)
      rules.canMoveBefore(state, ioA0, ioB0) must beTrue

    }
  }

  "rules.canInitiativeOrderPerform" ->- (baseMockups) should {

    // Should only be able to take actions during combat
    // The first for these actions is not element 0 of the other but whomever is the nextUp

    "work only when in combat" in {
      state.isCombatStarted returns false
      state.nextUp returns None
      rules.canInitiativeOrderPerform(state, ioA0, InitiativeTracker.action.StartRound) must beFalse
      there was one(state).isCombatStarted
      there was no(state).getInitiativeOrder
    }

    "only allow initiative actions during combat" in {
      state.isCombatStarted returns true
      state.nextUp returns Some(ioA1)
      state.initiativeTrackerFromID(ioA0) returns mock[InitiativeTracker]
      state.initiativeTrackerFromID(ioA1) returns mock[InitiativeTracker]

      rules.canInitiativeOrderPerform(state, ioA0, InitiativeTracker.action.StartRound) must beFalse
      there was one(state).isCombatStarted then
              atLeastOne(state).nextUp
    }

    "defer to InitiativeTracker with the correct first InitiativeTracker set" in {
      val mIT = mock[InitiativeTracker]
      val fIT = mock[InitiativeTracker]
      val action = InitiativeTracker.action.StartRound


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

  "rules.areAllied" ->- (baseMockups) should {
    "be true if combatant are both character" in {
      mockCombA.definition returns combatantDefinitionWithTypeOnly(cidA, CombatantType.Character)
      mockCombB.definition returns combatantDefinitionWithTypeOnly(cidB, CombatantType.Character)
      rules.areAllied(state, cidA, cidB) must beTrue
      there was one(mockCombA).definition
      there was one(mockCombB).definition
    }

    "be true if combatant are both are not character" in {
      mockCombA.definition returns combatantDefinitionWithTypeOnly(cidA, CombatantType.Monster)
      mockCombB.definition returns combatantDefinitionWithTypeOnly(cidB, CombatantType.Minion)
      rules.areAllied(state, cidA, cidB) must beTrue

      mockCombA.definition returns combatantDefinitionWithTypeOnly(cidA, CombatantType.Monster)
      mockCombB.definition returns combatantDefinitionWithTypeOnly(cidB, CombatantType.Monster)
      rules.areAllied(state, cidA, cidB) must beTrue

      mockCombA.definition returns combatantDefinitionWithTypeOnly(cidA, CombatantType.Minion)
      mockCombB.definition returns combatantDefinitionWithTypeOnly(cidB, CombatantType.Minion)
      rules.areAllied(state, cidA, cidB) must beTrue
    }

    "be false otherwise" in {
      mockCombA.definition returns combatantDefinitionWithTypeOnly(cidA, CombatantType.Character)
      mockCombB.definition returns combatantDefinitionWithTypeOnly(cidB, CombatantType.Minion)
      rules.areAllied(state, cidA, cidB) must beFalse

      mockCombB.definition returns combatantDefinitionWithTypeOnly(cidB, CombatantType.Monster)
      rules.areAllied(state, cidA, cidB) must beFalse
    }

  }

  /**
   * Helper to create a  CombatantRosterDefinition with only the ctype and eid defined.
   */
  def combatantDefinitionWithTypeOnly(comb: CombatantID, ctype: CombatantType.Value) =
    CombatantRosterDefinition(comb, null, CombatantEntity(null, null, null, 0, ctype, null))
}