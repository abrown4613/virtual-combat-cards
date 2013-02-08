/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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

import scala.language.postfixOps
import xml.{NodeSeq, Node}
import vcc.advtools.Monster._
import vcc.dndi.reader.{NoUsage, SomeUsage, Usage}
import vcc.dndi.reader.Parser.{Key, Text, Part}
import vcc.dndi.common.FormattedText._

class PowerReader(power: Node) extends XmlReaderMixin {

  private case class Attack(xml: Node, bonuses: List[AttackBonus],
                            hit: AttackResult, miss: AttackResult, effect: AttackResult)

  private case class AttackResult(attacks: List[Attack], name: Option[String], damage: Option[String], description: Option[String]) {
    def formatted(indent: Int): List[Line] =
      if (damage.isDefined || description.isDefined) {
        val lines = description.getOrElse("damage.").split("\n")
        List(makeLine(indent, formatHeader(name.get), (" " + formatOption(damage, "%s ") + lines.head))) ++
          lines.tail.map(makeLine(indent + 1, _))
      } else
        Nil
  }

  private case class AttackBonus(defense: String, bonus: Int) {
    def formatted: String = "%+d vs. %s".format(bonus, defense)
  }

  def read(): Power = {
    val action = (power \ "Action" text)
    Power(
      power \ "Name" text,
      action,
      extractUsage((power \ "Usage" text), (power \ "UsageDetails" text)),
      extractAttackType(power \ "Type" text, (power \ "IsBasic" text) == "true"),
      extractKeywords(power),
      getDescription(
        extractAttacks(power),
        action, optionalValue(power \ "Trigger")))
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

    def formatOptional(indent: Int, key: String, opt: Option[String]): List[Line] =
      opt.map(value => makeLine(indent, formatHeader(key), (" " + value))).toList

    def formatFixed(indent: Int, key: String, value: String) = List(makeLine(indent, formatHeader(key), " " + value))

    def nodeIfPresent(nodes: NodeSeq, fmt: Node => List[Line]) = nodes.headOption.map(node => fmt(node)).getOrElse(Nil)

    def extractFailedSavingThrows(baseNode: Node): List[AttackResult] =
      List("First Failed Saving Throw", "Second Failed Saving Throw", "Third Failed Saving Throw").
        zip(baseNode \ "FailedSavingThrows" \ "MonsterAttackEntry").
        map(p => extractResult(p._2, p._1))

    def extractResultsAndFormat(nodes: NodeSeq, indent: Int) = nodeIfPresent(nodes, extractResult(_).formatted(indent))

    def formatEffectAttacks(indent: Int, attack: Attack): List[Line] = {
      val header = optionalValue(attack.xml \ "Name").getOrElse("Effect")
      if (attack.effect.description.isDefined) {
        formatFixed(indent, header, attack.effect.description.get) ++
          extractFailedSavingThrows(attack.xml \ "Effect" head).flatMap(_.formatted(indent + 1)) ++
          extractResultsAndFormat(attack.xml \ "Effect" \ "Aftereffects" \ "MonsterAttackEntry", indent + 1)
      } else {
        formatAttack(indent + 1, attack)
      }
    }

    def formatAttackWithAttackBonus(indent: Int, attack: Attack): List[Line] =
      formatFixed(indent, formatOption(optionalValue(attack.xml \ "Name"), "%s", "Attack") + triggerAction,
        formatAttackDetails(attack)) ++
        attack.hit.formatted(indent) ++
        attack.miss.formatted(indent) ++
        extractResultsAndFormat(attack.xml \ "Hit" \ "Aftereffects" \ "MonsterAttackEntry", indent + 1) ++
        extractFailedSavingThrows(attack.xml \ "Hit" head).flatMap(_.formatted(indent + 1))

    def formatEffectOnlyAttack(indent: Int, attack: Attack): List[Line] =
      formatFixed(indent, "Effect" + triggerAction, attack.effect.description.get) ++
        extractFailedSavingThrows(attack.xml \ "Effect" head).flatMap(_.formatted(indent + 1)) ++
        extractResultsAndFormat(attack.xml \ "Effect" \ "Aftereffects" \ "MonsterAttackEntry", indent + 1)

    def formatSustains(indent: Int, attack: Attack) =
      nodeIfPresent(attack.xml \ "Effect" \ "Sustains" \ "MonsterSustainEffect",
        node => extractResult(node, "Sustain " + (node \ "Action" text)).formatted(indent + 1))

    def formatSecondaryAttacks(indent: Int, attack: Attack) =
      attack.hit.attacks.flatMap(a => formatAttack(indent + 1, a))

    def formatFirstAttackBlock(indent: Int, attack: Attack) =
      if (!attack.bonuses.isEmpty)
        formatAttackWithAttackBonus(indent, attack)
      else
        formatEffectOnlyAttack(indent, attack)

    def formatAttack(indent: Int, attack: Attack): List[Line] =
      formatFirstAttackBlock(indent, attack) ++
        formatSecondaryAttacks(indent, attack) ++
        formatSustains(indent, attack) ++
        attack.effect.attacks.flatMap(a => formatEffectAttacks(indent, a))

    Block(formatOptional(0, "Trigger", trigger) ++
      formatOptional(0, "Requirement", optionalValue(power \ "Requirements")) ++
      attacks.flatMap(attack => formatAttack(0, attack)))
  }

  private def extractAttacks(node: Node) = (node \ "Attacks" \ "MonsterAttack").map(extractAttack).toList

  private def extractAttackType(rangeType: String, isBasic: Boolean): AttackType =
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
      extractAttacks(node),
      nameOption,
      emptyOrStringAsOption(node \ "Damage" \ "Expression" text),
      emptyOrStringAsOption(node \ "Description" text))

  private def formatHeader(value: String) = "%s:".format(value)

  private def extractAttack(node: Node): Attack =
    Attack(
      node,
      extractAttackBonuses(node),
      extractResult(node \ "Hit" head),
      extractResult(node \ "Miss" head),
      extractResult(node \ "Effect" head))

  private def extractUsage(usage: String, usageDetail: String): Usage = {
    val converted: List[Part] = (usage, emptyOrStringAsOption(usageDetail)) match {
      case ("Recharge", Some(num)) if ("123456".contains(num)) => List(Text(usage + " " + num))
      case (s, o) => List(Key(s)) ++ o.map(Text(_))
    }
    SomeUsage.unapply(converted).getOrElse(NoUsage)
  }

  private def makeLine(indent: Int, header: String, text: String) = Line(indent, Seq(Italic(header), Normal(text)))

  private def makeLine(indent: Int, text: String) = Line(indent, Seq(Normal(text)))
}