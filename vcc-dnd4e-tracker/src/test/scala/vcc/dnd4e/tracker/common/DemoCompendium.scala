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
 * This trait defined a set of CombatEntity to be used in testing.
 */
trait DemoCompendium {
  val goblinDefinition = makeDefinition("Goblin", 2, 28, CombatantType.Monster)

  def makeDefinition(name: String, init: Int, hp: Int, ctype: CombatantType.Value) = {
    CombatantEntity(
      eid = "dummy:" + name,
      name = name,
      healthDef = ctype match {
        case CombatantType.Monster => MonsterHealthDefinition(hp)
        case CombatantType.Minion => MinionHealthDefinition
        case CombatantType.Character => CharacterHealthDefinition(hp)
      },
      initiative = init,
      ctype = ctype,
      statBlock = "<html><body>" + name + "</body></html>"
    )
  }
}

trait CommonCombatantID {
  val combA = CombatantID("A")
  val combB = CombatantID("B")
  val combC = CombatantID("C")
  val combD = CombatantID("D")
  val combE = CombatantID("E")

  val ioA0 = InitiativeOrderID(combA, 0)
  val ioA1 = InitiativeOrderID(combA, 1)
  val ioB0 = InitiativeOrderID(combB, 0)
  val ioC0 = InitiativeOrderID(combC, 0)
  val ioD0 = InitiativeOrderID(combD, 0)
  val ioE0 = InitiativeOrderID(combE, 0)
}
