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
package vcc.dnd4e.domain.tracker.snapshot

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.domain.tracker.common._

class CombatStateTest extends SpecificationWithJUnit with Mockito with CombatStateSnapshotHelper[String] {
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

  "a CombatStateView" should {
    "provide sequence of all effects" in {
      val mCombA = mock[CombatantStateView]
      val mCombB = mock[CombatantStateView]
      val badCondition = Effect.Condition.Generic("any", false)
      val goodCondition = Effect.Condition.Generic("any", true)
      val eff1 = Effect(EffectID(combA, 0), combA, goodCondition, Duration.Stance)
      val eff2 = Effect(EffectID(combA, 1), combA, goodCondition, Duration.Rage)
      val eff3 = Effect(EffectID(combB, 2), combA, badCondition, Duration.Rage)
      val aBigList = EffectList(combA, List(eff1, eff2))
      val aShortList = EffectList(combB, List(eff3))
      val cs = CombatState(
        false,
        null,
        List(ioa0), Map(ioa0 -> ita0),
        Some(ioa0),
        Map(combA -> mCombA, combB -> mCombB))
      mCombA.effects returns aBigList
      mCombB.effects returns aShortList

      val effects = cs.allEffects()

      effects.size must_== 3
      effects contains eff1
      effects contains eff2
      effects contains eff3
   }
  }
}