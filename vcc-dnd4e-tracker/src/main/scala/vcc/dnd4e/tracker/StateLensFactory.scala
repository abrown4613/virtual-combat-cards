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
//$Id$
package vcc.dnd4e.tracker

import common.{Combatant, CombatState}
import vcc.scalaz.Lens
import vcc.dnd4e.domain.tracker.common.{CombatantID, HealthTracker}

trait LensFactory[S]

trait StateLensFactory extends LensFactory[CombatState] {
  protected val combatantHealthLens: Lens[Combatant, HealthTracker]
  protected val combatantMapLens: Lens[CombatState, Map[CombatantID, Combatant]]

  def combatantHealth(cid: CombatantID): Lens[CombatState, HealthTracker] = {
    combatantMapLens.at(cid) compose combatantHealthLens
  }

  def combatant(cid: CombatantID): Lens[CombatState, Combatant] = {
    combatantMapLens.at(cid)
  }
}

object StateLensFactory extends StateLensFactory {
  protected val combatantHealthLens = Lens[Combatant, HealthTracker](
    c => c.health,
    (c, h) => c.copy(health = h)
  )
  protected val combatantMapLens: Lens[CombatState, Map[CombatantID, Combatant]] = Lens(s => s.roster, (state, r) => state.copy(roster = r))
}
