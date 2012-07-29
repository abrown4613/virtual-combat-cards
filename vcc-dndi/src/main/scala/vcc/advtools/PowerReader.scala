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

  private case class Attack(xml: Node, bonuses: List[AttackBonus],
                            hit: AttackResult, miss: AttackResult, effect: AttackResult)

  private case class AttackResult(attacks: List[Attack], name: Option[String], damage: Option[String], description: Option[String]) {
    def formatted(indent: Int): List[(String, String)] =
      if (damage.isDefined || description.isDefined) {
        val lines = description.getOrElse("damage.").split("\n")
        List(makeIndent(indent, name.get) -> (" " + formatOption(damage, "%s ") + lines.head)) ++
          lines.tail.map("\t" * (indent + 1) -> _)
      } else
        Nil
  }

  private case class AttackBonus(defense: String, bonus: Int) {
    def formatted: String = "%+d vs. %s".format(bonus, defense)
  }

  def read(): Power = {
    val powerName = (power \ "Name" text)
    val action = (power \ "Action" text)
    val usage = extractUsage((power \ "Usage" text), (power \ "UsageDetails" text))
    val rangeType = (power \ "Type" text)
    val isBasicAttack = (power \ "IsBasic" text) == "true"
    val keywords = extractKeywords(power)
    val attacks = (power \ "Attacks" \ "MonsterAttack").map(extractAttack)
    val trigger = optionalValue(power \ "Trigger")
    Power(powerName, action, usage, attackType(rangeType, isBasicAttack), keywords,
      getDescription(attacks, action, trigger))
  }

  private def getDescription(attacks: Seq[Attack], action: String, trigger: Option[String]): Block = {

    val triggerAction = if (trigger.isDefined) " (%s)".format(action) else ""

    def formatAttackDetails(attack: Attack): String = {
      val description = optionalValue(attack.xml \ "Description")
      val ms = emptyOrStringAsOption(
        (optionalValue(attack.xml \ "Range") ++ optionalValue(attack.xml \ "Targets").map("(%s)".format(_))).
          mkString(" "))
      formatOption(ms, "%s; ") + attack.bonuses(0).formatted + description.getOrElse("")
    }

    def formatOptional(indent: Int, key: String, opt: Option[String]) = {
      opt.map(value => makeIndent(indent, key) -> (" " + value))
    }

    def formatFixed(indent: Int, key: String, value: String) = Some(makeIndent(indent, key) -> (" " + value))

    def nodeIfPresent(optNode: Option[Node], fmt: Node => (String, String)) = optNode.map(node => fmt(node))

    def extractFailedSavingThrows(baseNode: Node): List[AttackResult] = {
      List("First Failed Saving Throw", "Second Failed Saving Throw", "Third Failed Saving Throw").
        zip(baseNode \ "FailedSavingThrows" \ "MonsterAttackEntry").
        map(p => extractResult(p._2, p._1))
    }

    def formatEffectAttacks(indent: Int, attack: Attack): List[(String, String)] = {
      val header = optionalValue(attack.xml \ "Name").getOrElse("Effect")
      if (attack.effect.description.isDefined) {
        Nil ++
          formatFixed(indent, header, attack.effect.description.get) ++
          extractFailedSavingThrows(attack.xml \ "Effect" head).flatMap(_.formatted(indent + 1)) ++
          nodeIfPresent((attack.xml \ "Effect" \ "Aftereffects" \ "MonsterAttackEntry").headOption,
            n => extractResult(n).formatted(indent + 1).head)
      } else {
        formatAttack(indent + 1, attack)
      }
    }

    def formatAttack(indent: Int, attack: Attack): List[(String, String)] = {
      Nil ++ (
        if (!attack.bonuses.isEmpty) {
          formatFixed(indent, formatOption(optionalValue(attack.xml \ "Name"), "%s", "Attack") + triggerAction,
            formatAttackDetails(attack)) ++
            attack.hit.formatted(indent) ++
            attack.miss.formatted(indent) ++
            nodeIfPresent((attack.xml \ "Hit" \ "Aftereffects" \ "MonsterAttackEntry").headOption,
              n => extractResult(n).formatted(indent + 1).head) ++
            extractFailedSavingThrows(attack.xml \ "Hit" head).flatMap(_.formatted(indent + 1))
        } else {
          formatFixed(indent, "Effect" + triggerAction, attack.effect.description.get) ++
            extractFailedSavingThrows(attack.xml \ "Effect" head).flatMap(_.formatted(indent + 1)) ++
            nodeIfPresent((attack.xml \ "Effect" \ "Aftereffects" \ "MonsterAttackEntry").headOption,
              n => extractResult(n).formatted(indent + 1).head)
        }) ++
        attack.hit.attacks.flatMap(a => formatAttack(indent + 1, a)) ++
        nodeIfPresent(attack.xml \ "Effect" \ "Sustains" \ "MonsterSustainEffect" headOption,
          node => extractResult(node, "Sustain " + (node \ "Action" text)).formatted(indent + 1).head)
    }

    val ls: List[(String, String)] =
      formatOptional(0, "Trigger", trigger).toList ++
        formatOptional(0, "Requirement", optionalValue(power \ "Requirements")).toList ++
        attacks.flatMap {
          attack =>
            formatAttack(0, attack) ++
              attack.effect.attacks.flatMap(a => formatEffectAttacks(0, a))
        }

    Block(
      for ((header, text) <- ls) yield {
        val indent = header.toSeq.takeWhile(_ == '\t').length
        val headerText = header.substring(indent)
        if (headerText == "")
          Line(indent, Seq(Normal(text)))
        else
          Line(indent, Seq(Italic(headerText), Normal(text)))
      })
  }

  private def attackType(rangeType: String, isBasic: Boolean): AttackType =
    if (rangeType == "None" || rangeType == "") {
      NonAttack
    } else {
      if (isBasic) BasicAttack(rangeType) else NormalAttack(rangeType)
    }

  private def extractKeywords(power: Node): Set[String] =
    (power \ "Keywords" \ "ObjectReference").map(getReferencedObjectName).toSet

  private def extractAttackBonuses(attack: Node): List[AttackBonus] =
    (attack \ "AttackBonuses" \ "MonsterPowerAttackNumber").map(x =>
      AttackBonus(
        x \ "Defense" \ "ReferencedObject" \ "DefenseName" text,
        (x \ "@FinalValue").text.toInt)).toList

  private def extractResult(node: Node): AttackResult = extractResult(node, emptyOrStringAsOption(node \ "Name" text))

  private def extractResult(node: Node, name: String): AttackResult = extractResult(node, Some(name))

  private def extractResult(node: Node, nameOption: Option[String]): AttackResult =
    AttackResult(
      (node \ "Attacks" \ "MonsterAttack").map(extractAttack).toList,
      nameOption,
      emptyOrStringAsOption(node \ "Damage" \ "Expression" text),
      emptyOrStringAsOption(node \ "Description" text))

  private def formatOption(option: Option[String], fmt: String, default: String = "") =
    option.map(fmt.format(_)).getOrElse(default)

  private def optionalValue(ns: NodeSeq): Option[String] = if (ns.isEmpty) None else emptyOrStringAsOption(ns(0).text)

  private def makeIndent(indent: Int, value: String) = "%s%s:".format("\t" * indent, value)

  private def extractAttack(node: Node): Attack = {
    Attack(
      node,
      extractAttackBonuses(node),
      extractResult(node \ "Hit" head),
      extractResult(node \ "Miss" head),
      extractResult(node \ "Effect" head)
    )
  }

  private def extractUsage(usage: String, usageDetail: String): Usage = {
    val converted: List[Part] = (usage, emptyOrStringAsOption(usageDetail)) match {
      case ("Recharge", Some(num)) if ("123456".contains(num)) => List(Text(usage + " " + num))
      case (s, o) => List(Key(s)) ++ o.map(Text(_))
    }
    SomeUsage.unapply(converted).getOrElse(NoUsage)
  }

  private def emptyOrStringAsOption(name: String): Option[String] = if (name == "") None else Some(name)

  private def getReferencedObjectName(node: Node): String = (node \ "ReferencedObject" \ "Name").text
}