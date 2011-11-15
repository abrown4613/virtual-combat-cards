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

import vcc.dnd4e.tracker.common.CombatState
import vcc.tracker.{Ruling, RulingProvider}

class AutomaticRulingProvider extends RulingProvider[CombatState] {
  def provideRulingFor(state: CombatState, rulingNeedingDecision: List[Ruling[CombatState, _, _]]): List[Ruling[CombatState, _, _]] = {
    for (ruling <- rulingNeedingDecision) yield {
      ruling match {
        case r: NextUpRuling => r.withDecision(r.candidates.next)
        case r: SaveRuling => r.withDecision(SaveRulingResult.Saved)
        case r: SaveSpecialRuling => r.withDecision(SaveSpecialRulingResult.Saved)
        case r: SaveVersusDeathRuling => r.withDecision(SaveVersusDeathResult.Failed)
        case r: SustainEffectRuling => r.withDecision(SustainEffectRulingResult.Sustain)
        case r: OngoingDamageRuling => r.withDecision(0)
        case r: RegenerationRuling => r.withDecision(0)
        case r => throw new Exception("Unknown ruling " + r)
      }
    }
  }
}