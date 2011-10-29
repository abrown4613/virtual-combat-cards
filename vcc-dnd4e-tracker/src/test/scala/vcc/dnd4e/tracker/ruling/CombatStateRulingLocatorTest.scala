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
package vcc.dnd4e.tracker.ruling

import org.specs2.mutable.SpecificationWithJUnit
import vcc.dnd4e.tracker.common.Effect.Condition
import vcc.dnd4e.tracker.common.{EffectID, Duration, CombatState}
import vcc.dnd4e.tracker.event.{ApplyDamageEvent, AddEffectEvent, EventSourceSampleEvents}
import vcc.dnd4e.tracker.transition.{StartRoundCommand, EndRoundCommand}
import vcc.tracker.Ruling

class CombatStateRulingLocatorTest extends SpecificationWithJUnit with EventSourceSampleEvents {

  private val durationSaveEnd = Duration.SaveEnd
  private val durationSaveEndSpecial = Duration.SaveEndSpecial
  private val durationEoTSA = Duration.RoundBound(ioA0, Duration.Limit.EndOfTurnSustain)
  private val aCondition = Condition.Generic("bad", false)
  private val anotherCondition = Condition.Generic("bad -> worst", false)
  private val regenerateCondition = Condition.Generic("regenerate 10 while bloodied", false)
  private val ongoingCondition = Condition.Generic("ongoing 5 fire", false)

  private val state = CombatState.empty.transitionWith(List(
    evtAddCombA, evtAddCombNoId, evtInitA, evtStart,
    AddEffectEvent(combA, combA, aCondition, durationSaveEnd),
    AddEffectEvent(combA, combA, anotherCondition, durationSaveEndSpecial),
    AddEffectEvent(comb1, combA, aCondition, durationEoTSA),
    AddEffectEvent(combA, combA, aCondition, durationEoTSA),
    AddEffectEvent(combA, combA, ongoingCondition, durationSaveEnd),
    AddEffectEvent(combA, combA, regenerateCondition, durationSaveEnd)
  ))

  private val eidA1 = EffectID(combA, 1)
  private val eidA2 = EffectID(combA, 2)
  private val eidA3 = EffectID(combA, 3)
  private val eidA4 = EffectID(combA, 4)
  private val eidA5 = EffectID(combA, 5)
  private val eid1_1 = EffectID(comb1, 1)

  "CombatStateRulingLocator" should {
    "Detect Save on end of round" in {
      (CombatStateRulingLocator.rulingsFromStateWithCommand(state, EndRoundCommand(ioA0))
        must contain(SaveRuling(eidA1, None)))
    }
    "Detect Save Special on end of round" in {
      (CombatStateRulingLocator.rulingsFromStateWithCommand(state, EndRoundCommand(ioA0))
        must contain(SaveSpecialRuling(eidA2, None)))
    }
    "Detect all sustains on end of round" in {
      (CombatStateRulingLocator.rulingsFromStateWithCommand(state, EndRoundCommand(ioA0))
        must contain(SustainEffectRuling(eidA3, None),
        SustainEffectRuling(eid1_1, None)))
    }
    "Detect save versus death when appropriate" in {
      val nState = state.transitionWith(List(ApplyDamageEvent(combA, 41)))
      (CombatStateRulingLocator.rulingsFromStateWithCommand(nState, EndRoundCommand(ioA0))
        must contain(SaveVersusDeathRuling(combA, None)))
    }

    "Detect Ongoing damage" in {
      (CombatStateRulingLocator.rulingsFromStateWithCommand(state, StartRoundCommand(ioA0))
        must contain(OngoingDamageRuling(eidA4, None)))
    }

    "Detect Regeneration damage" in {
      (CombatStateRulingLocator.rulingsFromStateWithCommand(state, StartRoundCommand(ioA0))
        must contain(RegenerationRuling(eidA5, None)))
    }

    "Detect regeneration first" in {
      type R = Ruling[CombatState, _, _, _]
      val regen: List[R] = List(RegenerationRuling(eidA5, None),
        OngoingDamageRuling(eidA4, None))
      val detected: List[R] = CombatStateRulingLocator.rulingsFromStateWithCommand(state, StartRoundCommand(ioA0))
      detected must contain(regen(0))
      detected must contain(regen(1))
      (detected.indexOf(regen(0)) must  beLessThan(detected.indexOf(regen(1))))
    }
  }
}