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
package vcc.dnd4e.tracker.dispatcher

import org.specs2.mutable.SpecificationWithJUnit
import vcc.dnd4e.tracker.common.Effect.Condition
import vcc.dnd4e.tracker.common.{EffectID, Duration, CombatState}
import vcc.dnd4e.tracker.transition.{EndRoundCommand}
import vcc.dnd4e.tracker.ruling._
import vcc.dnd4e.tracker.event.{ApplyDamageEvent, AddEffectEvent, EventSourceSampleEvents}

class CombatStateRulingLocatorTest extends SpecificationWithJUnit with EventSourceSampleEvents {

  private val durationSaveEnd = Duration.SaveEnd
  private val durationSaveEndSpecial = Duration.SaveEndSpecial
  private val durationEoTSA = Duration.RoundBound(ioA0, Duration.Limit.EndOfTurnSustain)
  private val aCondition = Condition.Generic("bad", false)
  private val anotherCondition = Condition.Generic("bad -> worst", false)

  private val state = CombatState.empty.transitionWith(List(
    evtAddCombA, evtAddCombNoId, evtInitA, evtStart,
    AddEffectEvent(combA, combA, aCondition, durationSaveEnd),
    AddEffectEvent(combA, combA, anotherCondition, durationSaveEndSpecial),
    AddEffectEvent(comb1, combA, aCondition, durationEoTSA),
    AddEffectEvent(combA, combA, aCondition, durationEoTSA)
  ))

  private val eidA1 = EffectID(combA, 1)
  private val eidA2 = EffectID(combA, 2)
  private val eidA3 = EffectID(combA, 3)
  private val eid1_1 = EffectID(comb1, 1)

  println("State roster : " + state.roster)
  println("State order: " + state.order)
  state.roster.combatant(combA).effects.effects.foreach(x => println("  --> " + x))

  "CombatStateRulingLocator" should {
    "Detect Save on end of round" in {
      (CombatStateRulingLocator.rulingsFromStateWithCommand(state, EndRoundCommand(ioA0))
        must contain(SaveRuling(Save.Against(eidA1, "bad"), None)))
    }
    "Detect Save Special on end of round" in {
      (CombatStateRulingLocator.rulingsFromStateWithCommand(state, EndRoundCommand(ioA0))
        must contain(SaveSpecialRuling(SaveSpecial.Against(eidA2, "bad -> worst"), None)))
    }
    "Detect all sustains on end of round" in {
      (CombatStateRulingLocator.rulingsFromStateWithCommand(state, EndRoundCommand(ioA0))
        must contain(SustainEffectRuling(SustainEffect.ToSustain(eidA3), None),
        SustainEffectRuling(SustainEffect.ToSustain(eid1_1), None)))
    }
    "Detect save versus death when appropriate" in {
      val nState = state.transitionWith(List(ApplyDamageEvent(combA, 41)))
      (CombatStateRulingLocator.rulingsFromStateWithCommand(nState, EndRoundCommand(ioA0))
        must contain(SaveVersusDeathRuling(SaveVersusDeath.Dying(combA), None)))
    }
  }
}