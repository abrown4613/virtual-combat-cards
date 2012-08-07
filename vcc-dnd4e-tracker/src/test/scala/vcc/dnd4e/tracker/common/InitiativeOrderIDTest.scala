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

import org.specs2.SpecificationWithJUnit

class InitiativeOrderIDTest extends SpecificationWithJUnit {
  private val combA = CombatantID("A")
  private val comb1 = CombatantID("1")

  def is =
    "  xml notation to string 1" ! (InitiativeOrderID(combA, 2).toXMLNotation must_== "o-A-2") ^
      "xml notation to string 2" ! (InitiativeOrderID(comb1, 0).toXMLNotation must_== "o-1-0") ^
      "from xml notation 1" ! (InitiativeOrderID.fromXMLNotation("o-1-0") must_== Some(InitiativeOrderID(comb1, 0))) ^
      "from xml notation 2" ! (InitiativeOrderID.fromXMLNotation("o-A-3") must_== Some(InitiativeOrderID(combA, 3))) ^
      end
}