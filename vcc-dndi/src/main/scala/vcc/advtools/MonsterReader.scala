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
import vcc.advtools.Monster.{Defense, GroupTaxonomy, BestiaryTaxonomy}
import xml.{NodeSeq, Node, XML}

class MonsterReader(inputStream: InputStream) {
  val xml = XML.load(inputStream)

  def getName: String = getElementAsText("Name")

  def getGroupCategory= GroupTaxonomy(getRole, getGroupRole, getIsLeader, getLevel, getExperience)

  def getTaxonomy = BestiaryTaxonomy(getSize, getOrigin, getType)

  def getDefense = {
    val map = extractValues(xml \ "Defenses" \\ "SimpleAdjustableNumber")
    Defense(map("AC"), map("Fortitude"), map("Reflex"), map("Will"))
  }

  def getSkills:Map[String, Int] = extractValues(xml \ "Skills" \\ "SkillNumber")

  private def getLevel: Int = getTextAtPath("Level").toInt

  private def getExperience = getTextAtPath("Experience", "@FinalValue").toInt

  private def getRole: String = getReferencedObjectName("Role")

  private def getGroupRole: String = getReferencedObjectName("GroupRole")

  private def getIsLeader: Boolean = getTextAtPath("IsLeader") == "true"

  private def getSize: String = getReferencedObjectName("Size")

  private def getType: String = getReferencedObjectName("Type")

  private def getOrigin: String = getReferencedObjectName("Origin")

  private def extractValues(ns: NodeSeq): Map[String, Int] = {
    ns.map(node => ((node \ "Name").text -> (node \ "@FinalValue").text.toInt)).toMap
  }

  private def getElementAsText(key: String): String = {
    (xml \ key).text
  }

  private def getReferencedObjectName(key: String): String = {
    (xml \ key \ "ReferencedObject" \ "Name").text
  }

  private def getTextAtPath(pathFragments: String*): String = {
    pathFragments.foldLeft[Node](xml)((node, path) => (node \ path)(0)).text
  }
}