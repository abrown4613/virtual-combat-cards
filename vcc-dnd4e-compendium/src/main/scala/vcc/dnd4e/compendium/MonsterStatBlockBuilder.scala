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
package vcc.dnd4e.compendium

import vcc.advtools.{Monster, MonsterReader}
import vcc.infra.xtemplate.TemplateDataSource
import xml.{Text, NodeSeq}
import vcc.advtools.Monster.{AbilityScores, BaseStats}

class MonsterStatBlockBuilder(monsterReader: MonsterReader) {

  private class BridgeTemplate(rootKey: String) extends TemplateDataSource {
    /**
     * Return a data source for a nested TemplateDataSource.
     */
    def templateGroup(key: String): List[TemplateDataSource] = List(new BridgeTemplate(rootKey + "." + key))

    /**
     * Return a XML NodeSeq for a give name.
     */
    def templateInlineXML(key: String): NodeSeq = Text("INLINE{" + rootKey + ":" + key + "}")

    /**
     * String value for a given key.
     */
    def templateVariable(key: String): Option[String] = {
      Some("VAR[" + rootKey + "." + key + "]")
    }
  }

  class Mapper(delegate: TemplateDataSource) extends TemplateDataSource {

    val defense = monsterReader.getDefense

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

    private val baseMap = init() ++ makeDefense(defense) ++ makeBaseStats(monsterReader.getBaseStats) ++
      makeImmunity(monsterReader) ++ makeEndBlock(monsterReader) ++ makeStats(monsterReader.getAbilityScores, monsterReader.getGroupCategory.level)

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

    private def init(): Map[String, String] = Map(
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
        delegate.templateVariable(key)
    }

    /**
     * Return a data source for a nested TemplateDataSource.
     */
    def templateGroup(key: String): List[TemplateDataSource] = {
      delegate.templateGroup(key)
    }

    /**
     * Return a XML NodeSeq for a give name.
     */
    def templateInlineXML(key: String): NodeSeq = {
      delegate.templateInlineXML(key)
    }
  }

  def render(): String = {
    val template = CaptureTemplateEngine.getInstance.fetchClassTemplate(Compendium.monsterClassID.shortClassName())
    val mySource = new Mapper(new BridgeTemplate("ROOT"))
    template.render(mySource).toString()
  }
}