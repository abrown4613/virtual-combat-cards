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
//$Id$
package vcc.dnd4e.domain.tracker.transactional

import org.specs.SpecificationWithJUnit
import vcc.dnd4e.domain.tracker.common._

class RulingSearchServiceTest extends SpecificationWithJUnit with MockCombatContextSpecification {

  //Provides a bunch of mock and a rState
  setupMockContext()

  val combA = CombatantID("A")
  val combB = CombatantID("B")

  //val simpleSaveEnd = Effect()

  val effects = makeEffects(combA, Duration.Stance, Duration.SaveEnd) ::: makeEffects(combB, Duration.SaveEnd)
  val saveSpecialEffects = makeEffects(combA, Duration.Stance, Duration.SaveEndSpecial) ::: makeEffects(combB, Duration.SaveEndSpecial)

  import RulingSearchService._

  val mState = mock[CombatState]

  //Mock combatant health
  val mockComb = mock[CombatantStateView]
  val mockHealth = mock[HealthTracker]
  mockComb.healthTracker returns mockHealth
  mockHealth.status returns HealthTracker.Status.Ok
  mState.allEffects returns Nil
  mState.combatantViewFromID(combA) returns mockComb

  "searchEndRound" should {
    "scan all effects" in {
      mState.allEffects returns effects
      searchEndRound(mState, combA)
      there was one(mState).allEffects()
    }

    "return the effect that a save will end form the combatant ending the round" in {
      mState.allEffects returns effects
      var ruling = searchEndRound(mState, combA).map(_.ruling)
      ruling must_== List(SaveEffectRuling.fromEffect(effects(1)))
    }

    "return the effect that a save will end special form the combatant ending the round" in {
      mState.allEffects returns saveSpecialEffects
      var ruling = searchEndRound(mState, combA).map(_.ruling)
      ruling must_== List(SaveEffectSpecialRuling.fromEffect(saveSpecialEffects(1)))
    }

    "return save versus death if the combatant is dying" in {
      mockHealth.status returns HealthTracker.Status.Dying
      var ruling = searchEndRound(mState, combA).map(_.ruling)
      ruling must_== List(SaveVersusDeathRuling(combA))
    }
  }

  private def makeEffects(comb: CombatantID, durations: Duration*) = {
    durations.zipWithIndex.map(p => Effect(EffectID(comb, p._2), comb, Effect.Condition.Generic("Effect " + p._2, false), p._1)).toList
  }
}