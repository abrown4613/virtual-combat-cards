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

import java.io.InputStream
import xml.{NodeSeq, Node, XML}
import vcc.advtools.Monster._
import util.matching.Regex

class MonsterReader(inputStream: InputStream) {
  val xml = XML.load(inputStream)

  def getName: String = getElementAsText("Name")

  def getCompendiumID: Option[Int] = extractWithRegex((xml \ "CompendiumUrl").text, """.*\?id=(\d+)""".r).map(_.toInt)

  def getGroupCategory = GroupTaxonomy(getRole, getGroupRole, getIsLeader, getLevel, getExperience)

  def getTaxonomy = BestiaryTaxonomy(getSize, getOrigin, getType, getKeyword, getRace)

  def getSenses: Option[String] = {
    def formatSense(item: String, qty: String): String = if (qty == "0") item else item + " " + qty

    def formatSenses(ns: NodeSeq): Seq[String] = {
      ns.map(node => formatSense((node \\ "Name").text, (node \ "Range").text))
    }
    val ns = formatSenses(xml \ "Senses" \\ "SenseReference")
    stringSeqToCommaSeparatedStringOption(ns)
  }

  def getDefense = {
    val map = extractValues(xml \ "Defenses" \\ "SimpleAdjustableNumber")
    Defense(map("AC"), map("Fortitude"), map("Reflex"), map("Will"))
  }

  def getSkills: Map[String, Int] = extractValues(xml \ "Skills" \\ "SkillNumber")

  def getAbilityScores = {
    val map = extractValues(xml \ "AbilityScores" \\ "AbilityScoreNumber")
    AbilityScores(map("Strength"), map("Constitution"), map("Dexterity"),
      map("Intelligence"), map("Wisdom"), map("Charisma"))
  }

  def getEquipment: Option[String] = {
    def formatItem(item: String, qty: String): String = if (qty == "1") item else item + " (" + qty + ")"

    def formatItems(ns: NodeSeq): Seq[String] = {
      ns.map(node => formatItem((node \\ "Name").text, (node \ "Quantity").text))
    }

    val ns = formatItems(xml \ "Items" \\ "ItemAndQuantity")
    stringSeqToCommaSeparatedStringOption(ns)
  }

  def getLanguages: Option[String] = {
    val ns = (xml \ "Languages" \\ "Name").map(_.text)
    stringSeqToCommaSeparatedStringOption(ns)
  }

  def getAlignment = getReferencedObjectName("Alignment")

  def getBaseStats = BaseStats(
    hitPoint = getIntAtPath("HitPoints", "@FinalValue"),
    initiative = getIntAtPath("Initiative", "@FinalValue"),
    actionPoints = getIntAtPath("ActionPoints", "@FinalValue"),
    saveBonus = getIntAtPath("SavingThrows", "MonsterSavingThrow", "@FinalValue")
  )

  def getSusceptibilities: List[Susceptibility] = {
    def mapSusceptibilities(key: String, builder: (String, Int) => Susceptibility): Seq[Susceptibility] = {
      (xml \ key \ "CreatureSusceptibility" map (node =>
        builder(
              getReferencedObjectName(node),
              (node \\ "@FinalValue" text).toInt)))
    }

    (mapSusceptibilities("Resistances", Resistance.apply) ++
      mapSusceptibilities("Weaknesses", Vulnerability.apply) ++
      (xml \ "Immunities" \\ "Name").map(node => Immune(node.text))
      ).toList
  }

  private def extractWithRegex(text: String, re: Regex): Option[String] = {
    text match {
      case `re`(m) => Some(m)
      case _ => None
    }
  }

  private def stringSeqToCommaSeparatedStringOption(ns: Seq[String]): Option[String] = {
    if (ns.isEmpty)
      None
    else
      Some(ns.mkString(", "))
  }

  private def getLevel: Int = getIntAtPath("Level")

  private def getExperience = getIntAtPath("Experience", "@FinalValue")

  private def getIntAtPath(pathFragments: String*): Int = getTextAtPath(pathFragments: _*).toInt

  private def getRole: String = getReferencedObjectName("Role")

  private def getGroupRole: String = getReferencedObjectName("GroupRole")

  private def getIsLeader: Boolean = getTextAtPath("IsLeader") == "true"

  private def getSize: String = getReferencedObjectName("Size")

  private def getType: String = getReferencedObjectName("Type")

  private def getOrigin: String = getReferencedObjectName("Origin")

  private def getRace: Option[String] = {
    val name = getReferencedObjectName("Race")
    emptyOrStringAsOption(name)
  }

  private def emptyOrStringAsOption(name: String): Option[String] = {
    if (name == "")
      None
    else
      Some(name)
  }

  private def getKeyword: Option[String] = {
    val names = (xml \ "Keywords" \\ "Name").map(_.text)
    stringSeqToCommaSeparatedStringOption(names)
  }

  private def extractValues(ns: NodeSeq): Map[String, Int] = {
    ns.map(node => ((node \ "Name").text -> (node \ "@FinalValue").text.toInt)).toMap
  }

  private def getElementAsText(key: String): String = {
    (xml \ key).text
  }

  private def getReferencedObjectName(key: String): String = {
    (xml \ key \ "ReferencedObject" \ "Name").text
  }

  private def getReferencedObjectName(node: Node): String = {
    (node \ "ReferencedObject" \ "Name").text
  }

  private def getTextAtPath(pathFragments: String*): String = {
    pathFragments.foldLeft[Node](xml)((node, path) => (node \ path)(0)).text
  }
}