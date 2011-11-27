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
package vcc.dnd4e.view.ruling

import vcc.dnd4e.tracker.common._

trait SampleStateData {
  protected val goblinEntity = CombatantEntity(null, "Goblin", MonsterHealthDefinition(24), 0, CombatantType.Minion, null)
  protected val pcEntity = CombatantEntity(null, "Fighter", CharacterHealthDefinition(40), 1, CombatantType.Character, null)
  protected val combA = CombatantID("A")
  protected val ioiA0 = InitiativeOrderID(combA, 0)
  protected val combB = CombatantID("B")
  protected val ioiB0 = InitiativeOrderID(combB, 0)
  protected val eidA1 = EffectID(combA, 1)
  protected val eidB1 = EffectID(combB, 1)
  protected val eidB2 = EffectID(combB, 2)
  protected val eidB3 = EffectID(combB, 3)
}