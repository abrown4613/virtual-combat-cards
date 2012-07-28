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

case class Attack(xml: Node, bonuses: List[AttackBonus], range: Option[String], targets: Option[String],
                  hit: AttackResult, miss: AttackResult, effect: AttackResult)

case class AttackResult(attacks: List[Attack], name: Option[String], damage: Option[String], description: Option[String])

case class AttackBonus(defense: String, bonus: Int)

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

  private def getDescription(attack: Attack, action: String, trigger: Option[String]): Block = {

    def formatAttackDetails(attack: Attack): String = {
      val description = optionalValue(attack.xml \ "Description")
      val ms = (attack.range ++ attack.targets.map("(%s)".format(_))).mkString("", " ", "; ")
      (if (ms == "; ") "" else ms) + formatBonus(attack.bonuses(0)) + description.getOrElse("")
    }

    def formatBonus(bonus: AttackBonus): String = "%+d vs. %s".format(bonus.bonus, bonus.defense)

    def formatResult(result: AttackResult, indent: Int = 0): Option[(String, String)] = {
      if (result.damage.isDefined || result.description.isDefined)
        Some("%s%s:".format("\t" * indent, result.name.get) -> (" " + result.damage.map(_ + " ").getOrElse("") + result.description.getOrElse("damage.")))
      else
        None
    }

    def formatOptional(indent: Int, key: String, opt: Option[String]): Option[(String, String)] = {
      opt.map(value => "%s%s:".format("\t" * indent, key) -> (" " + value))
    }

    def formatFixed(key: String, value: String): Option[(String, String)] = Some((key + ":") -> (" " + value))


    def nodeIfPresent(optNode: Option[Node], fmt: Node => (String, String)): Option[(String, String)] =
      optNode.map(node => fmt(node))

    def extractFailedSavingThrows(baseNode: Node): List[AttackResult] = {
      def extractSaveResult(t: (String, Node)): AttackResult = {
        AttackResult(Nil,
          Some(t._1),
          emptyOrStringAsOption(t._2 \ "Damage" \ "Expression" text),
          emptyOrStringAsOption(t._2 \ "Description" text)
        )
      }
      List("First Failed Saving Throw", "Second Failed Saving Throw", "Third Failed Saving Throw").
        zip(baseNode \ "FailedSavingThrows" \ "MonsterAttackEntry").map(extractSaveResult)
    }

    def formatBeholderEffect(indent: Int, attack: Attack): List[(String, String)] = {
      var ls: List[(String, String)] = Nil
      val header = optionalValue(attack.xml \ "Name").getOrElse("Effect")
      ls = ls ++ formatFixed(header, attack.effect.description.get) ++
        extractFailedSavingThrows(attack.xml \ "Effect" head).flatMap(formatResult(_, indent + 1)) ++
        nodeIfPresent((attack.xml \ "Effect" \ "Aftereffects" \ "MonsterAttackEntry").headOption,
          n => formatResult(extractResult(n), indent + 1).get)
      ls
    }

    def formatAttack(indent: Int, attack: Attack): List[(String, String)] = {
      var ls: List[(String, String)] = Nil
      if (!attack.bonuses.isEmpty) {
        ls = ls ++
          formatFixed(if (indent > 0) "\tSecondary Attack" else "Attack", formatAttackDetails(attack)) ++
          formatResult(attack.hit, indent) ++
          formatResult(attack.miss, indent) ++
          nodeIfPresent((attack.xml \ "Hit" \ "Aftereffects" \ "MonsterAttackEntry").headOption,
            n => formatResult(extractResult(n), indent + 1).get) ++
          extractFailedSavingThrows(attack.xml \ "Hit" head).flatMap(formatResult(_, indent + 1))
      } else {
        val header = trigger.map(x => "Effect (" + action + ")").getOrElse("Effect")
        ls = ls ++ formatFixed(header, attack.effect.description.get)
      }
      ls ++ attack.hit.attacks.flatMap(a => formatAttack(indent + 1, a))
    }

    var ls: List[(String, String)] = Nil
    val requirement = optionalValue(power \ "Requirements")
    ls = formatOptional(0, "Trigger", trigger).toList ++
      formatOptional(0, "Requirement", requirement).toList

    ls = ls ++ formatAttack(0, attack) ++
      attack.effect.attacks.flatMap(a => formatBeholderEffect(0, a))

    if (ls.isEmpty) null
    else Block(ls.map(l => {
      val indent = l._1.toSeq.takeWhile(_ == '\t').length
      Line(indent, Seq(Italic(l._1.substring(indent)), Normal(l._2)))
    }))
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
      emptyOrStringAsOption(result \ "Name" text),
      emptyOrStringAsOption(result \ "Damage" \ "Expression" text),
      emptyOrStringAsOption(result \ "Description" text)
    )
  }

  private def optionalValue(ns: NodeSeq): Option[String] = if (ns.isEmpty) None else emptyOrStringAsOption(ns(0).text)

  private def extractAttack(attack: Node): Attack = {
    Attack(
      attack,
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