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

import org.specs2.SpecificationWithJUnit
import vcc.dnd4e.tracker.transition.HealCommand
import vcc.dnd4e.tracker.event.{EventSourceSampleEvents}
import vcc.dnd4e.tracker.common.{EffectID}

class RegenerationRulingTest extends SpecificationWithJUnit with EventSourceSampleEvents {
  def is =
    "OngoingDamageRuling".title ^
      "have answer" ! e1 ^
      "have a user prompt" ! e4 ^
      "produce command no command when zero" ! e2 ^
      "produce heal command when set to greater than 0" ! e3 ^
      end

  private val eid = EffectID(combA, 1)
  private val zeroRegenRuling = RegenerationRuling(eid, Some(0))
  private val realRegenRuling = RegenerationRuling(eid, Some(7))
  private val state = emptyState.transitionWith(List(evtAddCombA, makeBadEndOfEncounterEffect(combA, combB, "regenerate 5")))

  private def e1 = {
    zeroRegenRuling.hasDecision must beTrue
  }

  private def e2 = {
    zeroRegenRuling.generateCommands(state) must_== Nil
  }

  private def e3 = {
    realRegenRuling.generateCommands(state) must_== List(HealCommand(combA, 7))
  }

  private def e4 = {
    zeroRegenRuling.userPrompt(state) must_== "Fighter [A] affected by: regenerate 5"
  }
}