/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.advtools

object Monster {

  case class BestiaryTaxonomy(size: String, origin: String, creatureType: String, keyword: Option[String], race: Option[String])

  case class GroupTaxonomy(role: String, groupRole: String, isLeader: Boolean, level: Int, experience: Int)

  case class Defense(ac: Int, fortitude: Int, reflex: Int, will: Int)

  case class AbilityScores(strength: Int, dexterity: Int, constitution: Int, intelligence: Int, wisdom: Int, charisma: Int)

  case class BaseStats(hitPoint: Int, initiative: Int, actionPoints: Int, saveBonus: Int)

  trait Susceptibility {
    def damageType: String

    def amount: Int
  }

  case class Resistance(damageType: String, amount: Int) extends Susceptibility

  case class Vulnerability(damageType: String, amount: Int) extends Susceptibility

  case class Immune(damageType: String) extends Susceptibility {
    def amount = Integer.MAX_VALUE
  }

  case class Power(powerName: String, action: String, usage: String, rangeType: String, isBasicAttack:Boolean)
}