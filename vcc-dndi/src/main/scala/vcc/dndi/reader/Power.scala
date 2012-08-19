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
package vcc.dndi.reader

import vcc.infra.text.StyledText
import vcc.infra.xtemplate.TemplateDataSource
import scala.xml.{Text, NodeSeq}
import vcc.dndi.reader.Parser.IconType

object ActionType extends Enumeration {
  val Standard = Value("Standard Action")
  val Triggered = Value("Triggered Action")
  val Move = Value("Move Action")
  val Minor = Value("Minor Action")
  val Free = Value("Free Action")
  val No = Value("No Action")
  val Trait = Value("Trait")

  private val caseInsensitiveMap = this.values.map(x => x.toString.toLowerCase -> x).toMap

  /**
   * case insensitive match
   */
  def unapply(name: String): Option[this.Value] = caseInsensitiveMap.get(name.toLowerCase)
}

sealed trait Usage

case class AuraUsage(radius: String) extends Usage

/**
 * At-Will usage, may be limited or unlimited
 * @param detail Optional detail or limitation of the power usage
 */
case class AtWillUsage(detail: Option[String]) extends Usage

object AtWillUsage {
  def apply() = new AtWillUsage(None)

  def apply(detail: String) = new AtWillUsage(Some(detail))
}

/**
 * Encounter usage, may be limited or unlimited.
 * @param detail If greater then 0, power can be used repeat times in an encounter, otherwise it is unlimited.
 */
case class EncounterUsage(detail: Option[String]) extends Usage

object EncounterUsage {
  def apply(detail: String): EncounterUsage = EncounterUsage(Some(detail))

  def apply(): EncounterUsage = new EncounterUsage(None)
}

/**
 * Daily Usage power
 */
object DailyUsage extends Usage

/**
 * No real usage
 */
object NoUsage extends Usage

/**
 * Conditional recharge power
 * @param condition Textual condition required to recharge
 */
case class RechargeConditionalUsage(condition: String) extends Usage

/**
 * Random recharge
 * @param startDice Start of the recharge range
 */
case class RechargeDiceUsage(startDice: Int) extends Usage

/**
 * Service object to format Usage into a valid XML NodeSeq
 */
object UsageFormatter {
  private val prefix = Seq(<img src="x.gif"/>, Text(" "))

  def format(usage: Usage): NodeSeq = usage match {
    case AuraUsage(n) => prefix ++ Seq(<b>Aura</b>, Text(n.formatted(" %s")))
    case AtWillUsage(None) => prefix ++ Seq(<b>At-Will</b>)
    case AtWillUsage(detail) => prefix ++ Seq(<b>At-Will</b>, Text(" %s".format(detail.get)))
    case EncounterUsage(None) => prefix ++ Seq(<b>Encounter</b>)
    case EncounterUsage(Some(detail)) => prefix ++ Seq(<b>Encounter</b>, Text(" %s".format(detail)))
    case DailyUsage => prefix ++ Seq(<b>Daily</b>)
    case RechargeConditionalUsage(condition) => prefix ++ Seq(<b>Recharge</b>, Text(" " + condition))
    case RechargeDiceUsage(start) => prefix ++ Seq(<b>Recharge</b>, Text(" ")) ++ (start to 6).map(d => <img src={IconType.diceImage(d)}/>)
    case NoUsage => <b></b>
    case null => <b>[NULL]</b>
  }
}


abstract class PowerDefinition extends TemplateDataSource {
  val name: String
  val usage: Usage
  val icons: Seq[IconType.Value]
  val keyword: String

  def templateGroup(key: String): List[TemplateDataSource] = Nil

  def templateInlineXML(key: String): NodeSeq = {
    key match {
      case "usage" => UsageFormatter.format(usage)
      case "iconset-short" => NodeSeq.fromSeq(icons.map(icon => <img src={IconType.iconToImage(icon)}/>))
      case _ => Nil
    }
  }

  def templateVariable(key: String): Option[String] = {
    val s: String = key match {
      case "name" => name
      case "keyword" => keyword
      case _ => null
    }
    if (s != null) Some(s) else None
  }

}

case class CompletePowerDefinition(icons: Seq[IconType.Value], name: String, keyword: String, usage: Usage) extends PowerDefinition

case class LegacyPowerDefinition(icons: Seq[IconType.Value], name: String, actionUsage: String, keyword: String, usage: Usage) extends PowerDefinition {
  override def templateVariable(key: String): Option[String] =
    if (key == "action-usage")
      if (actionUsage != null) Some(actionUsage)
      else None
    else super.templateVariable(key)
}

case class Power(definition: PowerDefinition, action: ActionType.Value, description: StyledText) extends TemplateDataSource {
  def templateGroup(key: String): List[TemplateDataSource] = Nil

  def templateInlineXML(key: String): NodeSeq = {
    if (key == "description") description.toXHTML()
    else definition.templateInlineXML(key)
  }

  def templateVariable(key: String): Option[String] =
    if (key == "action") Some(action.toString)
    else definition.templateVariable(key)
}