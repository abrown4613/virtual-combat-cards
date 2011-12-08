/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.application

import java.io._
import scala.xml._
import vcc.dnd4e.tracker.common._

class CombatSaveFile {

  private val lineBreak = Text("\n")
  private val tabStop1 = Text("\t")
  private val tabStop2 = Text("\t\t")
  private val tabStop3 = Text("\t\t\t")

  def load(inputStream: InputStream): CombatState = {
    val xml = XML.load(inputStream)
    var state = CombatState.empty
    state = state.copy(
      comment = Option(getOptionalTextNode(xml, "comment")),
      roster = loadRoster(firstMatchingChild(xml, "roster"))
    )
    state
  }

  def save(outputStream: OutputStream, combatState: CombatState) {
    val writer: Writer = new PrintWriter(outputStream)
    val commentNode: NodeSeq = breakLine(combatState.comment.map(c => createSimpleDataNode("comment", c)).toList)
    val topLevel = lineBreak :: commentNode :: serializeCombatants(combatState.roster) :: Nil
    XML.write(writer, (<CombatSave>
      {topLevel}
    </CombatSave>), "UTF-8", true, null)
    writer.flush()
  }

  private def createHealthDefinition(healthDefinition: HealthDefinition): Elem = {
      <health-definition type={healthDefinition.combatantType.toString} hp={healthDefinition.totalHP.toString}/>
  }

  private def serializeCombatantEntity(entity: CombatantEntity) = {
    val nodes = Seq(
      createSimpleDataNode("entity-id", entity.eid),
      createSimpleDataNode("name", entity.name),
      createSimpleDataNode("initiative", entity.initiative.toString),
      createHealthDefinition(entity.healthDef),
      createSimpleDataNode("statblock", entity.statBlock))
    createSequenceNode("entity", lineBreak ++ formatPretty(tabStop3, nodes) ++ tabStop2)
  }

  private def formatPretty(indent: Text, nodes: Seq[Node]): Seq[Node] = {
    nodes.flatMap(node => indent ++ node ++ lineBreak)
  }

  private def serializeCombatant(values: Iterable[Combatant]): Seq[Node] = {
    values.map(combatant => {
      val seq = lineBreak ++ tabStop2 ++ serializeCombatantEntity(combatant.definition.entity) ++ lineBreak ++ tabStop1
      val baseNode = createSequenceNode("combatant", seq);

      if (combatant.definition.alias != null)
        baseNode % Attribute("id", Text(combatant.definition.cid.id), Null) % Attribute("alias", Text(combatant.definition.alias), Null)
      else
        baseNode % Attribute("id", Text(combatant.definition.cid.id), Null)
    }).toSeq
  }

  private def serializeCombatants(roster: Roster[Combatant]): NodeSeq = {
    val values = roster.entries.values
    createSequenceNode("roster", lineBreak ++ formatPretty(tabStop1, serializeCombatant(values)))
  }

  private def breakLine(nodeSeq: NodeSeq): NodeSeq = {
    if (!nodeSeq.isEmpty) tabStop1 ++ nodeSeq ++ lineBreak
    else nodeSeq
  }

  private def loadRoster(rosterNode: Node): Roster[Combatant] = {
    val rosterMap: Map[CombatantID, Combatant] =
      Map(rosterNode \ "combatant" map ({
        combatantNode: Node =>
          val combId = CombatantID(combatantNode \ "@id" text)
          val normAlias = getOptionalTextNode(combatantNode, "@alias")
          combId ->
            Combatant(CombatantRosterDefinition(combId, normAlias,
              parseCombatantEntity(firstMatchingChild(combatantNode, "entity"))))
      }): _*)
    Roster[Combatant](Combatant.RosterFactory, rosterMap)
  }

  private def firstMatchingChild(node: Node, tagName: String): Node = (node \ tagName)(0)

  private def getOptionalTextNode(baseNode: Node, tagName: String): String = {
    val search = baseNode \ tagName
    if (search.isEmpty) null
    else search.text
  }

  private def parseHealthDefinition(healthNode: Node): HealthDefinition = {
    val combatantType = (healthNode \ "@type" text)
    val hp = (healthNode \ "@hp" text).toInt
    combatantType match {
      case "Character" => CharacterHealthDefinition(hp)
      case "Monster" => MonsterHealthDefinition(hp)
      case "Minion" => MinionHealthDefinition
    }
  }

  private def parseCombatantEntity(combatantNode: Node): CombatantEntity = {
    CombatantEntity(combatantNode \ "entity-id" text,
      combatantNode \ "name" text,
      parseHealthDefinition(firstMatchingChild(combatantNode, "health-definition")),
      (combatantNode \ "initiative" text).toInt,
      combatantNode \ "statblock" text)
  }

  private def createSimpleDataNode(label: String, text: String): Elem = {
    Elem(null, label, Null, TopScope, Text(text))
  }

  private def createSequenceNode(label: String, nodes: Seq[Node]): Elem = {
    Elem(null, label, Null, TopScope, nodes: _*)
  }

}