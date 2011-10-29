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
import vcc.dnd4e.tracker.common.{EffectID}
import vcc.dnd4e.tracker.transition.CancelEffectCommand

class SaveRulingTest extends SpecificationWithJUnit with EventSourceSampleEvents {
  def is =
    "Ruling".title ^
      "have proper user prompt" ! e0 ^
      "have answer" ! e1 ^
      "produce answer" ! e2 ^
      "produce no answer if not saved" ! e3 ^
      end

  private val eid = EffectID(combA, 1)
  private val savedRuling = SaveRuling(eid, Some(SaveRulingResult.Saved))
  private val failedRuling = SaveRuling(eid, Some(SaveRulingResult.Failed))
  private val state = emptyState.transitionWith(List(evtAddCombA, makeBadEndOfEncounterEffect(combA, combB, "bad")))

  private def e0 = {
    savedRuling.userPrompt(state) must_== "Fighter [A] must make a saving throws against: bad"
  }

  private def e1 = {
    savedRuling.hasDecision must beTrue
  }

  private def e2 = {
    savedRuling.generateCommands(state) must_== List(CancelEffectCommand(eid))
  }

  private def e3 = {
    failedRuling.generateCommands(state) must_== Nil
  }
}