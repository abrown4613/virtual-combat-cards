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
package vcc.dnd4e.domain.tracker.common


import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}

@RunWith(classOf[JUnitSuiteRunner])
class EffectTransformationTest extends JUnit4(EffectTransformationSpec)

object EffectTransformationSpec extends Specification {
  val combA = CombatantID("A")
  val goodCondition = Effect.Condition.Generic("a condition", true)
  val badCondition = Effect.Condition.Generic("a condition", false)
  val iob = InitiativeOrderID(CombatantID("B"), 0)
  val ioa = InitiativeOrderID(combA, 0)
  val durationEoNT = Duration.RoundBound(iob, Duration.Limit.EndOfNextTurn)
  val durationEoT = Duration.RoundBound(iob, Duration.Limit.EndOfTurn)
  val durationSoNT = Duration.RoundBound(iob, Duration.Limit.StartOfNextTurn)
  val durationEoNTS = Duration.RoundBound(iob, Duration.Limit.EndOfNextTurnSustain)
  val durationEoTS = Duration.RoundBound(iob, Duration.Limit.EndOfTurnSustain)
  val saveEnd = Duration.SaveEnd

  val roundBoundDurations = List[Duration](durationEoNT, durationEoT, durationEoNTS, durationEoTS, durationSoNT)
  val saveDurations = List[Duration](Duration.SaveEnd, Duration.SaveEndSpecial)
  val restDurations = List[Duration](Duration.EndOfEncounter, Duration.Stance, Duration.Rage)
  val nonRoundBound = Duration.Other :: saveDurations ::: restDurations

  "a EffectTransformation.startRound" should {

    "advance effect bound to the InitiativeOrderID" in {
      val et = EffectTransformation.startRound(iob)
      et.transform(Effect(combA, badCondition, durationEoNT)).duration must_== durationEoT
      et.transform(Effect(combA, badCondition, durationEoNTS)).duration must_== durationEoTS
    }

    "not change effect bount to another InitiativeOrderID" in {
      val et = EffectTransformation.startRound(ioa)
      durationEoNT must_== durationEoNT
      et.transform(Effect(combA, badCondition, durationEoNT)).duration must_== durationEoNT
      et.transform(Effect(combA, badCondition, durationEoNTS)).duration must_== durationEoNTS
    }

    "cancel effect bound to the start of the round of a combatant" in {
      val et = EffectTransformation.startRound(iob)
      et.transform(Effect(combA, badCondition, durationSoNT)) must beNull
    }

    "not change effect bound to another InitiativeOrderID" in {
      val et = EffectTransformation.startRound(ioa)
      et.transform(Effect(combA, badCondition, durationSoNT)).duration must_== durationSoNT
    }

    "leave other unchanged" in {
      val et = EffectTransformation.startRound(ioa)
      for (dur <- nonRoundBound) {
        val eff = Effect(combA, badCondition, dur)
        et.transform(eff) must_== eff
      }
    }
  }

  "EffectTransformation.endRound" should {
    "end effect that are bound to the round" in {
      val et = EffectTransformation.endRound(iob)
      et.transform(Effect(combA, badCondition, durationEoT)) must beNull
      et.transform(Effect(combA, badCondition, durationEoTS)) must beNull
    }

    "not change effect bount to some other InitiativeOrderID" in {
      val et = EffectTransformation.endRound(ioa)
      val eff1 = Effect(combA, badCondition, durationEoT)
      val eff2 = Effect(combA, badCondition, durationEoTS)
      et.transform(eff1) must_== eff1
      et.transform(eff2) must_== eff2
    }

    "leave other unchanged" in {
      val et = EffectTransformation.endRound(ioa)
      for (dur <- nonRoundBound) {
        val eff = Effect(combA, badCondition, dur)
        et.transform(eff) must_== eff
      }
    }
  }
  "EffectTransformation.delayRound" should {
    "expire round bound effect that are beneficial to target when ally delays" in {
      val et = EffectTransformation.processDelay(true, iob)
      val eff = Effect(combA, goodCondition, durationEoT)
      et.transform(eff) must beNull
    }

    "keep round bound effect that are not beneficial to target when ally delays" in {
      val et = EffectTransformation.processDelay(true, iob)
      val eff = Effect(combA, badCondition, durationEoT)
      et.transform(eff) must_== eff
    }

    "expire effect that are sustained by delayer" in {
      val et = EffectTransformation.processDelay(false, iob)
      et.transform(Effect(combA, badCondition, durationEoTS)) must beNull
    }

    "preserve effect that are sustained by others" in {
      val et = EffectTransformation.processDelay(false, ioa)
      val eff1 = Effect(combA, badCondition, durationEoTS)
      val eff2 = Effect(combA, badCondition, durationEoNTS)
      et.transform(eff1) must_== eff1
      et.transform(eff2) must_== eff2
    }

    "expire round bound effects that are not beneficial to enemy" in {
      val et = EffectTransformation.processDelay(false, iob)
      val eff = Effect(combA, badCondition, durationEoT)

      et.transform(eff) must beNull
    }

    "keep round bound effects that are beneficial to enemy" in {
      val et = EffectTransformation.processDelay(false, iob)
      val eff = Effect(combA, goodCondition, durationEoT)
      et.transform(eff) must_== eff
    }

    "not change non end of round bound effects" in {
      val et = EffectTransformation.processDelay(true, ioa)
      val et2 = EffectTransformation.processDelay(true, ioa)
      val lst: List[Duration] = durationSoNT :: durationEoNT :: nonRoundBound
      for (dur <- lst) {
        val eff = Effect(combA, badCondition, dur)
        et.transform(eff) must_== eff
        et2.transform(eff) must_== eff
      }
    }
  }

  "EffectTransformation.applyRest" should {
    "expire Stance duration" in {
      val et = EffectTransformation.applyRest
      val eff = Effect(combA, badCondition, Duration.Stance)
      et.transform(eff) must beNull
    }

    "expire Rage duration" in {
      val et = EffectTransformation.applyRest
      val eff = Effect(combA, badCondition, Duration.Rage)
      et.transform(eff) must beNull
    }

    "expire EndOfEncounter" in {
      val et = EffectTransformation.applyRest
      val eff = Effect(combA, badCondition, Duration.EndOfEncounter)
      et.transform(eff) must beNull
    }

    "leave all other duration unchanged" in {
      val et = EffectTransformation.applyRest
      val lst: List[Duration] = Duration.Other :: saveDurations ::: roundBoundDurations
      for (dur <- lst) {
        val eff = Effect(combA, badCondition, dur)
        et.transform(eff) must_== eff
      }

    }
  }

  "EffectTransformation.updateCondition" should {
    val effectId = EffectID(combA, 10)
    val otherEffectId = EffectID(combA, 9)
    val oldCondition = Effect.Condition.Generic("old", false)
    val newCondition = Effect.Condition.Generic("new", false)
    val et = EffectTransformation.updateCondition(effectId, newCondition)
    val markCondition = Effect.Condition.Mark(combA, false)

    "update condition if not mark and identified by EffectID" in {
      val eff = Effect(effectId, combA, oldCondition, durationEoT)
      et.transform(eff) must_== Effect(effectId, combA, newCondition, durationEoT)
    }

    "leave mark unchanged if identified by EffectID" in {
      val eff = Effect(effectId, combA, markCondition, durationEoT)
      et.transform(eff) must_== eff

    }

    "leave other unchanged if not identified by EffectID" in {
      val eff = Effect(otherEffectId, combA, oldCondition, durationEoT)
      et.transform(eff) must_== eff
    }
  }

  "EffectTransformation.sustainEffect" should {
    val effectId = EffectID(combA, 10)
    val otherEffectId = EffectID(combA, 9)
    val anyCondition = Effect.Condition.Generic("old", false)
    val et = EffectTransformation.sustainEffect(effectId)

    "increase duration of a sustainable effect identified by EffectID" in {
      val eff = Effect(effectId, combA, anyCondition, durationEoTS)
      et.transform(eff) must_== Effect(effectId, combA, anyCondition, durationEoNTS)
    }

    "leave effect unchanged if identified by EffectID but not sustainable" in {
      val eff = Effect(effectId, combA, anyCondition, durationEoT)
      et.transform(eff) must_== eff
    }

    "leave effect unchanged if identified by EffectID and sustainable but not to expire" in {
      val eff = Effect(effectId, combA, anyCondition, durationEoNTS)
      et.transform(eff) must_== eff
    }

    "leave effect unchanged if not identified by EffectID but sustainable" in {
      val eff = Effect(otherEffectId, combA, anyCondition, durationEoNTS)
      et.transform(eff) must_== eff
    }

    "leave effect unchanged if not identified by EffectID" in {
      val eff = Effect(otherEffectId, combA, anyCondition, durationEoT)
      et.transform(eff) must_== eff

    }
  }

  "EffectTransformation.cancelEffect" should {
    val effectId = EffectID(combA, 10)
    val otherEffectId = EffectID(combA, 9)
    val anyCondition = Effect.Condition.Generic("old", false)
    val et = EffectTransformation.cancelEffect(effectId)

    "cancel effect if identified by EffectID" in {
      val eff = Effect(effectId, combA, anyCondition, durationEoT)
      et.transform(eff) must beNull
    }

    "do nothing if not identified by EffectID" in {
      val eff = Effect(otherEffectId, combA, anyCondition, durationEoT)
      et.transform(eff) must_== eff
    }
  }
}