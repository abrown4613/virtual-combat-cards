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
package vcc.dnd4e.tracker.common

/**
 * Use this trait to add a common set of combatant information
 */
trait SampleStateData {
  val combA = CombatantID("A")
  val ioA0 = InitiativeOrderID(combA, 0)
  val ioA1 = InitiativeOrderID(combA, 1)

  val combB = CombatantID("B")
  val ioB0 = InitiativeOrderID(combB, 0)

  val comb1 = CombatantID("1")
  val io1_0 = InitiativeOrderID(comb1, 0)

  val comb2 = CombatantID("2")
  val io2_0 = InitiativeOrderID(comb2, 0)

  //Val CombatantEntity
  val entityPc1 = CombatantEntity(null, "Fighter", CharacterHealthDefinition(40), 2, CombatantType.Character, null)
  val entityPc2 = CombatantEntity(null, "Mage", CharacterHealthDefinition(25), 4, CombatantType.Character, null)
  val entityMinion = CombatantEntity(null, "Minion", MinionHealthDefinition, 1, CombatantType.Minion, null)
  val entityMonster = CombatantEntity(null, "Monster", MonsterHealthDefinition(30), 1, CombatantType.Minion, null)
}