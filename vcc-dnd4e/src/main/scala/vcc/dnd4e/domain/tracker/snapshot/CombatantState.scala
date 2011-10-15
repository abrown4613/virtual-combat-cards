/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.snapshot

import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.common.EffectList
import vcc.dnd4e.domain.tracker.common.CombatantStateView

case class CombatantState(definition: CombatantRosterDefinition,
                          healthTracker: HealthTracker,
                          effects: EffectList,
                          comment: String) extends CombatantStateView {
  def name: String = definition.entity.name
}