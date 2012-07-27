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

import vcc.dndi.reader.Parser.IconType
import vcc.dndi.reader.Usage
import vcc.dndi.common.FormattedText.Block

object Monster {

  case class BestiaryTaxonomy(size: String, origin: String, creatureType: String, keyword: Option[String], race: Option[String]) {
    def singleDescription = (size + " " + origin + " " + creatureType +
      keyword.map(" (%s)".format(_)).getOrElse("") + race.map(", %s".format(_)).getOrElse("")).toLowerCase.capitalize
  }

  case class GroupTaxonomy(role: String, groupRole: String, isLeader: Boolean, level: Int, experience: Int) {
    def completeRole = normalizeRole(groupRole) + " " + role + (if (isLeader) " (Leader)" else "")

    private def normalizeRole(role: String) = role match {
      case "Standard" => ""
      case s => s
    }
  }

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

  sealed trait AttackType {

    def toIcon(name: String): Option[IconType.Value] = IconType.values.find(p => p.toString.toLowerCase == name.toLowerCase)

    def asIcon(): Seq[IconType.Value]
  }

  object BasicAttack {
    private val iconMapping = Map[String, IconType.Value](
      "melee" -> IconType.MeleeBasic,
      "area burst" -> IconType.AreaBasic,
      "close blast" -> IconType.CloseBasic,
      "close burst" -> IconType.CloseBasic,
      "ranged" -> IconType.RangeBasic,
      "area burst" -> IconType.AreaBasic
    )
  }
  case class BasicAttack(attackType: String) extends AttackType {
    def asIcon(): Seq[IconType.Value] = {
      attackType.split("\n").flatMap(n => BasicAttack.iconMapping.get(n.toLowerCase))
    }
  }

  object NormalAttack {
    private val iconMapping = Map[String, IconType.Value](
      "melee" -> IconType.Melee,
      "area burst" -> IconType.Melee,
      "close blast" -> IconType.Close,
      "close burst" -> IconType.Close,
      "ranged" -> IconType.Range,
      "area burst" -> IconType.Area
    )
  }

  case class NormalAttack(attackType: String) extends AttackType {

    def asIcon(): Seq[IconType.Value] = {
      attackType.split("\n").flatMap(n => NormalAttack.iconMapping.get(n.toLowerCase))
    }
  }

  case object NonAttack extends AttackType {
    def asIcon(): Seq[IconType.Value] = Nil
  }

  object Attack {
    def apply(hits: List[AttackBonus], damage: Option[String]): Attack = apply(hits, damage, None)

    def apply(hits: List[AttackBonus], damage: Option[String], description: Option[String]): Attack = {
      Attack(hits, None, None,
        AttackResult(List(), damage, description),
        AttackResult(List(), None, None),
        AttackResult(List(), None, None))
    }

    def apply(hits: List[AttackBonus], range:Option[String], targets:Option[String], damage: Option[String], description: Option[String]): Attack = {
      Attack(hits, range, targets,
        AttackResult(List(), damage, description),
        AttackResult(List(), None, None),
        AttackResult(List(), None, None))
    }
  }

  case class Attack(bonuses: List[AttackBonus], range: Option[String], targets:Option[String], hit: AttackResult, miss: AttackResult, effect: AttackResult)

  case class AttackResult(attacks: List[Attack], damage: Option[String], description: Option[String])

  case class AttackBonus(defense: String, bonus: Int)

  case class Power(powerName: String,
                   action: String, usage: Usage, attackType: AttackType,
                   keywords: Set[String], text: Block)

  trait BaseCreatureTrait

  case class Aura(name: String, radius: Int, details: String) extends BaseCreatureTrait

  case class CreatureTrait(name: String, details: String) extends BaseCreatureTrait

}