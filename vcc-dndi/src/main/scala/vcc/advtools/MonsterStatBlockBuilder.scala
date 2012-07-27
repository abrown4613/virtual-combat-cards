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

import vcc.infra.xtemplate.{Template, TemplateDataSource}
import xml.{Text, NodeSeq}
import vcc.advtools.Monster.{AbilityScores, BaseStats}
import vcc.dndi.reader.Parser.IconType
import vcc.infra.text.{TextSegment, TextBlock, StyledText}
import vcc.dndi.reader.UsageFormatter

class MonsterStatBlockBuilder(monsterReader: MonsterReader) {

  class PowerMapper(power: Monster.Power) extends TemplateDataSource {
    /**
     * String value for a given key.
     */
    def templateVariable(key: String): Option[String] = {
      key match {
        case "name" => Some(power.powerName)
        case "keyword" => if (power.keywords.isEmpty) None else Some(power.keywords.mkString("(", ", ", ")"))
        case _ =>
          None
      }
    }

    /**
     * Return a data source for a nested TemplateDataSource.
     */
    def templateGroup(key: String): List[TemplateDataSource] = Nil

    /**
     * Return a XML NodeSeq for a give name.
     */
    def templateInlineXML(key: String): NodeSeq = {
      def makeIcon(icon: IconType.Value) = <img src={IconType.iconToImage(icon)}/>
      key match {
        case "iconset-short" =>
          power.attackType.asIcon().map(makeIcon)
        case "usage" =>
          UsageFormatter.format(power.usage)
        case "description" =>
          PowerDescriptionFormatter.formatAttack(power.text).toXHTML()
        case _ =>
          Seq()
      }
    }
  }

  class AuraMapper(aura: Monster.Aura) extends TemplateDataSource {

    def templateVariable(key: String): Option[String] = {
      key match {
        case "name" => Some(aura.name)
        case "keyword" => None
        case _ => None
      }
    }

    def templateGroup(key: String): List[TemplateDataSource] = Nil

    def templateInlineXML(key: String): NodeSeq = {
      key match {
        case "iconset-short" =>
          NodeSeq.fromSeq(<img src={IconType.iconToImage(IconType.Aura)}/>)
        case "usage" =>
          NodeSeq.fromSeq(Seq(Text(" "), <img src={IconType.iconToImage(IconType.Separator)}/>, <b>Aura</b>, Text(" " + aura.radius)))
        case "description" =>
          StyledText(List(TextBlock("P", "flavorIndent", TextSegment(aura.details)))).toXHTML()
        case _ => NodeSeq.fromSeq(Nil)
      }
    }
  }

  class CreatureTraitMapper(aTrait: Monster.CreatureTrait) extends TemplateDataSource {

    def templateVariable(key: String): Option[String] = {
      key match {
        case "name" =>
          Some(aTrait.name)
        case _ =>
          None
      }
    }

    def templateGroup(key: String): List[TemplateDataSource] = Nil

    def templateInlineXML(key: String): NodeSeq = {
      key match {
        case "description" =>
          StyledText(List(TextBlock("P", "flavorIndent", TextSegment(aTrait.details)))).toXHTML()
        case _ => NodeSeq.fromSeq(Nil)
      }
    }
  }

  class BaseMapper() extends TemplateDataSource {
    private val powers = monsterReader.getPowers

    private val baseMap = makeBaseStat() ++ makeDefense(monsterReader.getDefense) ++ makeBaseStats(monsterReader.getBaseStats) ++
      makeImmunity(monsterReader) ++ makeEndBlock(monsterReader) ++ makeStats(monsterReader.getAbilityScores, monsterReader.getGroupCategory.level)

    def makeBaseStats(stats: BaseStats) = Map(
      "stat:initiative" -> stats.initiative.toString,
      "stat:hp" -> stats.hitPoint.toString,
      "stat:bloodied" -> (if (stats.hitPoint > 2) (stats.hitPoint / 2).toString else null),
      "stat:action points" -> (if (stats.actionPoints > 0) stats.actionPoints.toString else null),
      "stat:saving throws" -> (if (stats.saveBonus > 0) stats.actionPoints.formatted("%+d") else null)
    )

    private def makeStats(scores: AbilityScores, level: Int) = {
      def formatScore(value: Int) = "%d (%+d)".format(value, (value / 2 - 5) + level / 2)
      Map(
        "stat:str" -> formatScore(scores.strength),
        "stat:dex" -> formatScore(scores.dexterity),
        "stat:con" -> formatScore(scores.constitution),
        "stat:wis" -> formatScore(scores.wisdom),
        "stat:int" -> formatScore(scores.intelligence),
        "stat:cha" -> formatScore(scores.charisma)
      )
    }

    def makeDefense(defense: Monster.Defense): Map[String, String] = Map(
      "stat:ac" -> defense.ac.toString,
      "stat:reflex" -> defense.reflex.toString,
      "stat:will" -> defense.will.toString,
      "stat:fortitude" -> defense.fortitude.toString
    )

    private def makeImmunity(reader: MonsterReader) = {

      val sus = reader.getSusceptibilities

      def formatList(pf: PartialFunction[Monster.Susceptibility, String]): String = {
        val items = sus.collect(pf)
        if (items.isEmpty)
          null
        else
          items.mkString(", ")
      }

      Map(
        "stat:immune" -> formatList {
          case Monster.Immune(s) => s
        },
        "stat:resist" -> formatList {
          case Monster.Resistance(s, a) => s + " " + a.toString
        },
        "stat:vulnerable" -> formatList {
          case Monster.Vulnerability(s, a) => s + " " + a.toString
        }
      )
    }

    private def makeBaseStat(): Map[String, String] = Map(
      "base:name" -> monsterReader.getName,
      "stat:skills" -> monsterReader.getSkills.toSeq.map(p => "%s %+d".format(p._1, p._2)).mkString(", "),
      "stat:perception" -> "%+d".format(monsterReader.getSkills.getOrElse("Perception", 0)),
      "stat:senses" -> monsterReader.getSenses.getOrElse(null),
      "stat:speed" -> monsterReader.getSpeeds,
      "base:type" -> monsterReader.getTaxonomy.singleDescription,
      "base:role" -> monsterReader.getGroupCategory.completeRole
    )

    private def makeEndBlock(reader: MonsterReader) = Map(
      "stat:alignment" -> reader.getAlignment,
      "stat:equipment" -> reader.getEquipment.getOrElse(null),
      "stat:languages" -> reader.getLanguages.getOrElse("-"),
      "text:description" -> null,
      "text:comment" -> null
    )

    /**
     * String value for a given key.
     */
    def templateVariable(key: String): Option[String] = {
      if (baseMap.isDefinedAt(key))
        Option(baseMap.get(key).get)
      else
        None
    }

    /**
     * Return a data source for a nested TemplateDataSource.
     */
    def templateGroup(key: String): List[TemplateDataSource] = {

      def powerToTemplate(filterAction: String*) = {
        powers.filter(p => filterAction.contains(p.action)).map(power =>
          new PowerMapper(power))
      }
      key match {
        case "trait" =>
          monsterReader.getCreatureTraits.map(t => t match {
            case a: Monster.Aura => new AuraMapper(a)
            case ct: Monster.CreatureTrait => new CreatureTraitMapper(ct)
          })
        case "standard action" =>
          powerToTemplate("Standard")
        case "minor action" =>
          powerToTemplate("Minor")
        case "triggered action" =>
          powerToTemplate("Immediate Reaction", "No Action", "Free Action")

        case action =>
          println("Action: " + action)
          println("  *vs: " + powers.map(_.action).mkString(", "))
          Nil
      }
    }

    /**
     * Return a XML NodeSeq for a give name.
     */
    def templateInlineXML(key: String): NodeSeq = Seq()
  }

  def render(template: Template): String = {
    val mySource = new BaseMapper()
    template.render(mySource).toString()
  }
}