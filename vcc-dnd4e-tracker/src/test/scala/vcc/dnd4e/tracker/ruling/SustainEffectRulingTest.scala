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

import vcc.dnd4e.tracker.event.EventSourceSampleEvents
import vcc.dnd4e.tracker.transition.SustainEffectCommand
import org.specs2.specification.Fragments
import vcc.tracker.Ruling
import vcc.dnd4e.tracker.common.{CombatState, EffectID}

class SustainEffectRulingTest extends RulingAcceptance("SustainEffectRuling") with EventSourceSampleEvents {

  private val eid = EffectID(combA, 1)
  private val cancelRulings = SustainEffectRuling(eid, Some(SustainEffectRulingResult.Cancel))
  private val sustainRuling = SustainEffectRuling(eid, Some(SustainEffectRulingResult.Sustain))

  protected val state = emptyState.transitionWith(List(evtAddCombA, makeBadEndOfEncounterEffect(combA, combB, "effect")))
  protected val rulingWithAnswer: Ruling[CombatState, _, _, _] = cancelRulings
  protected val rulingWithoutAnswer: Ruling[CombatState, _, _, _] = SustainEffectRuling(eid, None)
  protected val userPromptMessage: String = "Sustain \"effect\" (from Fighter [A])"

  def buildCases: Fragments =
    "produce nothing on not sustained" ! (cancelRulings.generateCommands(state) must_== Nil) ^
      "produce command on sustained " ! (sustainRuling.generateCommands(state) must_== List(SustainEffectCommand(eid))) ^
      endp
}