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
package vcc.dnd4e.tracker.event

import org.specs2.SpecificationWithJUnit
import org.specs2.mock.Mockito
import vcc.dnd4e.tracker.common.Effect.Condition
import vcc.dnd4e.tracker.common.{EffectID, EffectTransformation, Duration}

class EffectEventTest extends SpecificationWithJUnit with EventSourceSampleEvents with Mockito {
  def is =
    "effect events".title ^
      "add effect" ! execEffectAdd ^
      "tranform effect list" ! transformEffectList ^
      end

  private val aDuration = Duration.EndOfEncounter
  private val aGoodCondition = Condition.Generic("good", true)

  private def stateWithMockEffectList = {
    val state = emptyState.transitionWith(List(evtAddCombA, evtAddCombNoId))
    state.lensFactory.combatantEffectList(combA).mod(state, el => spy(el))
  }

  private def execEffectAdd = {
    val event = AddEffectEvent(combA, combB, aGoodCondition, aDuration)
    val state = stateWithMockEffectList
    val eLens = state.lensFactory.combatantEffectList(combA)
    event.transition(state)
    (there was one(eLens.get(state)).addEffect(combB, aGoodCondition, aDuration))
  }

  private def transformEffectList = {
    val transform = EffectTransformation.sustainEffect(EffectID(combA, 1))
    val event = ChangeEffectListEvent(combA, transform)
    val state = stateWithMockEffectList
    val eLens = state.lensFactory.combatantEffectList(combA)
    event.transition(state)
    (there was one(eLens.get(state)).transformAndFilter(transform))
  }
}