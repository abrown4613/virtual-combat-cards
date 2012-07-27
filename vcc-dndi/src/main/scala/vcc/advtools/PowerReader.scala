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

import xml.{NodeSeq, Node}
import vcc.advtools.Monster._
import vcc.dndi.reader.{NoUsage, SomeUsage, Usage}
import vcc.dndi.reader.Parser.{Key, Text, Part}
import vcc.dndi.common.FormattedText._

class PowerReader(power: Node) {

  def read(): Power = {
    val powerName = (power \ "Name" text)
    val action = (power \ "Action" text)
    val usage = extractUsage((power \ "Usage" text), (power \ "UsageDetails" text))
    val rangeType = (power \ "Type" text)
    val isBasicAttack = (power \ "IsBasic" text) == "true"
    val keywords = extractKeywords(power)
    val attacks = (power \ "Attacks" \ "MonsterAttack").map(extractAttack)
    val trigger = optionalValue(power \ "Trigger")
    Power(powerName, action, usage, attackType(rangeType, isBasicAttack), keywords, getDescription(attacks(0), action, trigger))
  }

  private def getDescription(attack: Monster.Attack, action: String, trigger: Option[String]): Block = {

    def formatAttackDetails(attack: Monster.Attack): String = {
      val ms = (attack.range ++ attack.targets.map("(%s)".format(_))).mkString("", " ", "; ")
      (if (ms == "; ") "" else ms) + formatBonus(attack.bonuses(0))
    }

    def formatBonus(bonus: AttackBonus): String = "%+d vs. %s".format(bonus.bonus, bonus.defense)

    var ls: List[(String, String)] = Nil

    val requirement = optionalValue(power \ "Requirements")
    if (trigger.isDefined)
      ls = "Trigger: " -> trigger.get :: ls
    if (requirement.isDefined)
      ls = "Requirement" -> (": " + requirement.get) :: ls
    if (!attack.bonuses.isEmpty) {
      ls = "Attack" -> (": " + formatAttackDetails(attack)) :: ls
      if (attack.hit.damage.isDefined || attack.hit.description.isDefined)
        ls = "Hit" -> (": "+attack.hit.damage.map(_ + " ").getOrElse("") + attack.hit.description.getOrElse("damage.")) :: ls
    } else {
      val header = trigger.map(x => "Effect (" + action + "): ").getOrElse("Effect: ")
      ls = header -> attack.effect.description.get :: ls
    }
    if (ls.isEmpty) null
    else Block(ls.reverse.map(l => Line(0, Seq(Italic(l._1), Normal(l._2)))))
  }

  private def attackType(rangeType: String, isBasic: Boolean): AttackType = {
    if (rangeType == "None" || rangeType == "") {
      NonAttack
    } else {
      if (isBasic) BasicAttack(rangeType) else NormalAttack(rangeType)
    }
  }

  private def extractKeywords(power: Node): Set[String] = {
    (power \ "Keywords" \ "ObjectReference").map(getReferencedObjectName).toSet
  }

  private def extractAttackBonuses(attack: Node): List[AttackBonus] = {
    (attack \ "AttackBonuses" \ "MonsterPowerAttackNumber").map(x =>
      AttackBonus(
        x \ "Defense" \ "ReferencedObject" \ "DefenseName" text,
        (x \ "@FinalValue").text.toInt)).toList
  }

  private def extractResult(result: Node): AttackResult = {
    AttackResult(
      (result \ "Attacks" \ "MonsterAttack").map(extractAttack).toList,
      emptyOrStringAsOption(result \ "Damage" \ "Expression" text),
      emptyOrStringAsOption(result \ "Description" text)
    )
  }

  private def optionalValue(ns: NodeSeq): Option[String] = if (ns.isEmpty) None else emptyOrStringAsOption(ns(0).text)

  private def extractAttack(attack: Node): Attack = {
    Attack(
      extractAttackBonuses(attack),
      optionalValue(attack \ "Range"),
      optionalValue(attack \ "Targets"),
      extractResult(attack \ "Hit" head),
      extractResult(attack \ "Miss" head),
      extractResult(attack \ "Effect" head)
    )
  }

  private def extractUsage(usage: String, usageDetail: String): Usage = {

    val detail = emptyOrStringAsOption(usageDetail)

    val converted: List[Part] = (usage, detail) match {
      case ("Recharge", Some(num)) if ("123456".contains(num)) => List(Text(usage + " " + num))
      case (s, o) => List(Key(s)) ++ o.map(Text(_))
    }

    val ret = SomeUsage.unapply(converted).getOrElse(NoUsage)
    ret
  }

  private def emptyOrStringAsOption(name: String): Option[String] = {
    if (name == "")
      None
    else
      Some(name)
  }

  private def getReferencedObjectName(node: Node): String = {
    (node \ "ReferencedObject" \ "Name").text
  }
}