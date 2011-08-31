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
package vcc.dnd4e.domain.tracker.transactional

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import org.specs2.specification.Scope
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.domain.tracker.common._
import vcc.controller.CommandSource

class RulingSearchServiceTest extends SpecificationWithJUnit {

  trait mockContext extends Scope with Mockito {
    var aController: AbstractCombatController = null

    val mSource: CommandSource = mock[CommandSource]
    val rState: CombatContext = new CombatContext()

    val effects = makeEffects(combA, Duration.Stance, Duration.SaveEnd) ::: makeEffects(combB, Duration.SaveEnd)
    val saveSpecialEffects = makeEffects(combA, Duration.Stance, Duration.SaveEndSpecial) ::: makeEffects(combB, Duration.SaveEndSpecial)
    val mState = mock[CombatContext]

    //Mock combatant health
    val mockComb = mock[Combatant]
    val mockHealth = mock[HealthTracker]
    mockComb.health returns mockHealth
    mockHealth.status returns HealthStatus.Ok
    mState.allEffects returns Nil
    mState.combatantFromID(combA) returns mockComb
  }

  private val combA = CombatantID("A")
  private val combB = CombatantID("B")
  private val ioiA = InitiativeOrderID(combA, 0)

  import RulingSearchService._

  "searchEndRound" should {
    "scan all effects" in new mockContext{
      mState.allEffects returns effects
      searchEndRound(mState, combA)
      there was one(mState).allEffects
    }

    "return the effect that a save will end form the combatant ending the round" in new mockContext{
      mState.allEffects returns effects
      var ruling = searchEndRound(mState, combA).map(_.ruling)
      ruling must_== List(SaveEffectRuling.fromEffect(effects(1)))
    }

    "return the effect that a save will end special form the combatant ending the round" in new mockContext{
      mState.allEffects returns saveSpecialEffects
      var ruling = searchEndRound(mState, combA).map(_.ruling)
      ruling must_== List(SaveEffectSpecialRuling.fromEffect(saveSpecialEffects(1)))
    }

    "return save versus death if the combatant is dying" in new mockContext{
      mockHealth.status returns HealthStatus.Dying
      var ruling = searchEndRound(mState, combA).map(_.ruling)
      ruling must_== List(SaveVersusDeathRuling(combA))
    }

    "return sustain effect in all combatants" in new mockContext{
      val sustainable = Duration.RoundBound(ioiA, Duration.Limit.EndOfTurnSustain)
      mState.allEffects returns (makeEffects(combA, sustainable, Duration.EndOfEncounter) :::
        makeEffects(combB, Duration.EndOfEncounter, sustainable))
      var ruling = searchEndRound(mState, combA).map(_.ruling)
      ruling must_== List(SustainEffectRuling(EffectID(combA, 0), "Effect 0 (on A)"), SustainEffectRuling(EffectID(combB, 1), "Effect 1 (on B)"))
    }
  }

  "search startRound" should {

    val ongoing = makeEffectWithDesc(combA, ("ongoing 5 fire", Duration.SaveEnd)) ::: makeEffectWithDesc(combB, ("ongoing 5 fire", Duration.SaveEnd))
    val regenerate = makeEffectWithDesc(combA, ("regenerate 5", Duration.SaveEnd)) ::: makeEffectWithDesc(combB, ("regenerate 5", Duration.SaveEnd))

    "scan all effects" in new mockContext{
      mState.allEffects returns ongoing
      searchEndRound(mState, combA)
      there was one(mState).allEffects
    }

    "find ongoing damage ruling" in new mockContext{
      mState.allEffects returns ongoing
      var ruling = searchStartRound(mState, combA).map(_.ruling)
      ruling must_== List(OngoingDamageRuling(EffectID(combA, 0), "ongoing 5 fire", 5))
    }

    "find regenerate damage ruling" in new mockContext{
      mState.allEffects returns regenerate
      var ruling = searchStartRound(mState, combA).map(_.ruling)
      ruling must_== List(RegenerateByRuling(EffectID(combA, 0), "regenerate 5", 5))
    }

    "find regenerate before ongoing independent of order" in new mockContext{
      mState.allEffects returns (ongoing ::: regenerate)
      var ruling = searchStartRound(mState, combA).map(_.ruling)
      ruling must_== List(
        RegenerateByRuling(EffectID(combA, 0), "regenerate 5", 5),
        OngoingDamageRuling(EffectID(combA, 0), "ongoing 5 fire", 5))
    }
  }

  private def makeEffects(comb: CombatantID, durations: Duration*) = {
    durations.zipWithIndex.map(p => Effect(EffectID(comb, p._2), comb, Effect.Condition.Generic("Effect " + p._2, false), p._1)).toList
  }

  private def makeEffectWithDesc(comb: CombatantID, effectDuration: (String, Duration)*) = {
    effectDuration.zipWithIndex.map(p => Effect(EffectID(comb, p._2), comb, Effect.Condition.Generic(p._1._1, false), p._1._2)).toList
  }

}