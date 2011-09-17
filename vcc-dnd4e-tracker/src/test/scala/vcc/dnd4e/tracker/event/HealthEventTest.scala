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

class HealthEventTest extends SpecificationWithJUnit with EventSourceSampleEvents with Mockito {

  private def makeState() = {
    val s = emptyState.transitionWith(evtAddCombA :: Nil)
    s.lensFactory.combatantHealth(combA).mod(s, ht => spy(ht))
  }

  def is =
    "HealthEvent test".title ^
      "ApplyDamageEvent" ! damageEvent ^
      "ApplyHealingEvent" ! healEvent ^
      "SetTemporaryHitPointsEvent" ! tempHPEvent ^
      "FailDeathSaveEvent" ! failDeathEvent ^
      "RevertDeathEvent" ! revertDeathEvent ^
      end

  private def damageEvent = {
    val state = makeState()
    ApplyDamageEvent(combA, 10).transition(state)
    there was one(state.lensFactory.combatantHealth(combA).get(state)).applyDamage(10)
  }

  private def healEvent = {
    val state = makeState()
    ApplyHealingEvent(combA, 10).transition(state)
    there was one(state.lensFactory.combatantHealth(combA).get(state)).heal(10)
  }

  private def tempHPEvent = {
    val state = makeState()
    SetTemporaryHitPointsEvent(combA, 10).transition(state)
    there was one(state.lensFactory.combatantHealth(combA).get(state)).setTemporaryHitPoints(10)
  }

  private def failDeathEvent = {
    val state = makeState()
    FailDeathSaveEvent(combA).transition(state)
    there was one(state.lensFactory.combatantHealth(combA).get(state)).failDeathSave()
  }

  private def revertDeathEvent = {
    val state = makeState()
    RevertDeathEvent(combA).transition(state)
    there was one(state.lensFactory.combatantHealth(combA).get(state)).raiseFromDead()
  }
}