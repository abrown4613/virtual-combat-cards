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
import vcc.dnd4e.tracker.event.EventSourceSampleEvents
import vcc.dnd4e.tracker.common.EffectID
import vcc.dnd4e.tracker.transition.SustainEffectCommand

class SustainEffectRulingTest extends SpecificationWithJUnit with EventSourceSampleEvents {
  def is =
    "SustainEffectRuling".title ^
      "have valid user prompt" ! e0 ^
      "have answer" ! e1 ^
      "produce nothing on not sustained" ! e2 ^
      "produce command on sustained " ! e3 ^
      end

  private val eid = EffectID(combA, 1)
  private val cancelRulings = SustainEffectRuling(eid, Some(SustainEffectRulingResult.Cancel))
  private val sustainRuling = SustainEffectRuling(eid, Some(SustainEffectRulingResult.Sustain))
  private val state = emptyState.transitionWith(List(evtAddCombA, makeBadEndOfEncounterEffect(combA, combB, "effect")))

  private def e0 = {
    cancelRulings.userPrompt(state) must_== "Sustain \"effect\" (from Fighter [A])"
  }

  private def e1 = {
    cancelRulings.hasDecision must beTrue
  }

  private def e2 = {
    cancelRulings.generateCommands(state) must_== Nil
  }

  private def e3 = {
    sustainRuling.generateCommands(state) must_== List(SustainEffectCommand(eid))
  }
}