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
package vcc.dnd4e.tracker.common

import org.specs2.SpecificationWithJUnit
import org.specs2.mock.Mockito
import vcc.dnd4e.tracker.common.Effect.Condition

class CombatantTest extends SpecificationWithJUnit with Mockito with SampleStateData {

  private val combDef = CombatantRosterDefinition(combA, null, entityPc1)

  def is =
    "Combatant" ^
      "can apply short rest" ! execRest(false) ^
      "can apply extends rest" ! execRest(true) ^
      "update definition and preserve damage" ! execChangeDefinition ^
      generateDiff ^
      end

  private def execRest(isExtended: Boolean) = {
    val mHealth = mock[HealthTracker]
    val mEffects = mock[EffectList]
    val mComb = Combatant(combDef).copy(health = mHealth, effects = mEffects)

    mComb.applyRest(isExtended)

    (there was one(mEffects).transformAndFilter(EffectTransformation.applyRest)) and
      (there was one(mHealth).rest(isExtended))
  }

  private def generateDiff = {
    val comb = Combatant(combDef)
    "generates diff for" ^
      "comment" ! {
        val x = comb.copy(comment = "new comment")
        x.diff(comb) must_== Set(CombatantCommentDiff(combA, "new comment", ""))
      } ^
      "health" ! {
        val x = comb.copy(health = comb.health.applyDamage(10))
        x.diff(comb) must_== Set(CombatantDiff(combA, x.health, comb.health))
      } ^
      "effect" ! {
        val x = comb.copy(effects = comb.effects.addEffect(combB, Condition.Generic("a", true), Duration.EndOfEncounter))
        x.diff(comb) must_== Set(CombatantDiff(combA, x.effects, comb.effects))
      } ^
      "definition and health" ! {
        val x = comb.updateDefinition(CombatantRosterDefinition(combA, null, entityPc2))
        x.diff(comb) must_== Set(
          CombatantDiff(combA, x.definition, comb.definition),
          CombatantDiff(combA, x.health, comb.health))
      } ^
      endp
  }

  private def execChangeDefinition = {
    val comb = Combatant(combDef)
    val damage = 10
    val expectedNewHealth = HealthTracker.createTracker(entityPc2.healthDef).applyDamage(damage)
    val combWithNewDefinition = comb.
      copy(health = comb.health.applyDamage(damage)).
      updateDefinition(CombatantRosterDefinition(combA, null, entityPc2))
    (combWithNewDefinition.diff(comb) must_== Set(
      CombatantDiff(combA, combWithNewDefinition.definition, comb.definition),
      CombatantDiff(combA, expectedNewHealth, comb.health))) and
      (combWithNewDefinition.health.currentHP must_== expectedNewHealth.currentHP)
  }
}
