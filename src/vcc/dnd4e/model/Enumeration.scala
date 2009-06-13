/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.model

object InitiativeState extends Enumeration {
  val Reserve=Value("Reserve")
  val Ready=Value("Ready")
  val Readying=Value("Readying") // This state is when the combatant has readied but not ended it's round
  val Surprised=Value("Surprised")
  val Delaying=Value("Delaying")
  val Acting=Value("Acting")
  val Waiting=Value("Waiting")
}

object CombatantType extends Enumeration {
  val Minion=Value("Minion")
  val Character=Value("Character")
  val Monster=Value("Monster")
  
  def isCharacter(ctype:this.Value) = ctype==Character
}