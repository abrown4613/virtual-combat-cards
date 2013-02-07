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
package vcc.dnd4e.application

import language.postfixOps
import java.io._
import scala.xml._
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.util.{ReorderedListBuilderCompare, ReorderedListBuilder}
import vcc.dnd4e.tracker.common.Effect.Condition
import vcc.dnd4e.tracker.common.Duration.RoundBound

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
      roster = loadRoster(firstMatchingChild(xml, "roster")),
      order = loadOrder(firstMatchingChild(xml, "initiative-order"))
    )
    state
  }

  def save(outputStream: OutputStream, combatState: CombatState) {
    val writer: Writer = new OutputStreamWriter(outputStream, "UTF-8")
    val commentNode: NodeSeq = indentNodes(tabStop1, combatState.comment.map(c => createSimpleDataNode("comment", c)).toList)
    val topLevel = lineBreak :: commentNode :: serializeRoster(combatState.roster) :: serializeOrder(combatState.order) :: Nil
    XML.write(writer, createSequenceNode("combat-state", topLevel.flatMap(x => x)), "UTF-8", true, null)
    writer.flush()
  }

  private def serializeCombatants(values: Iterable[Combatant]): Seq[Node] = {
    values.map(combatant => serializeCombatant(combatant)).toSeq
  }

  private def serializeCombatant(combatant: Combatant) = {
    val parts = Seq(
      serializeCombatantEntity(combatant.definition.entity),
      createSimpleDataNode("comment", combatant.comment),
      serializeHealthDelta(combatant.health.getDelta)) ++ serializeEffects(combatant)

    val baseNode = createSequenceNode("combatant", lineBreak ++ indentNodes(tabStop2, parts) ++ tabStop1);
    val aliasAttribute = attributeOrNull("alias", combatant.definition.alias)
    baseNode % attributeOrNull("id", combatant.definition.cid.toXMLNotation) % aliasAttribute
  }

  private def serializeCombatantEntity(entity: CombatantEntity) = {
    val nodes = Seq(
      createSimpleDataNode("entity-id", entity.eid),
      createSimpleDataNode("name", entity.name),
      createSimpleDataNode("initiative", entity.initiative.toString),
      serializeHealthDefinition(entity.healthDef),
      createSimpleDataNode("statblock", entity.statBlock))
    createSequenceNode("entity", lineBreak ++ indentNodes(tabStop3, nodes) ++ tabStop2)
  }

  private def serializeHealthDefinition(healthDefinition: HealthDefinition): Elem = {
      <health-definition type={healthDefinition.combatantType.toString} hp={healthDefinition.totalHP.toString}/>
  }

  private def serializeHealthDelta(delta: HealthTrackerDelta): Elem = {
      <health-delta damage={delta.damage.toString}
                    temporary={delta.temporaryHP.toString}
                    death-strikes={delta.deathStrikes.toString}/>
  }

  private def serializeEffects(combatant: Combatant): NodeSeq = {
    combatant.effects.effects.map(serializeEffect)
  }

  private def serializeEffect(effect: Effect): Node = {
    def serializeCondition(condition: Condition): Elem = {
      condition match {
        case Condition.Generic(description, beneficial) =>
          createSimpleDataNode("generic", description) % attributeOrNull("beneficial", beneficial.toString)
        case Condition.Mark(marker, permanent) =>
            <mark by={marker.toXMLNotation} permanent={permanent.toString}/>
      }
    }

    def serializeDuration(duration: Duration): Seq[Attribute] = {
      duration match {
        case RoundBound(ioi, limit) => Seq(
          Attribute(null, "limit-order-id", ioi.toXMLNotation, Null),
          Attribute(null, "limit", limit.toString, Null))
        case staticDuration => Seq(Attribute(null, "duration", staticDuration.shortDescription, Null))
      }
    }

    val effectElem = createSequenceNode("effect", serializeCondition(effect.condition))
    val durationAttributes = serializeDuration(effect.duration) ++ Seq(
      attributeOrNull("effect-number", effect.effectId.seq.toString),
      attributeOrNull("source", effect.source.toXMLNotation))
    durationAttributes.foldLeft(effectElem)(_ % _)
  }

  private def serializeRoster(roster: Roster[Combatant]): NodeSeq = {
    val values = roster.entries.values
    createSequenceNode("roster", lineBreak ++ indentNodes(tabStop1, serializeCombatants(values)))
  }

  private def parseHealthDefinition(healthNode: Node): HealthDefinition = {
    val combatantType = (healthNode \ "@type" text)
    val hp = findTagInNodeAsInt(healthNode, "@hp")
    combatantType match {
      case "Character" => CharacterHealthDefinition(hp)
      case "Monster" => MonsterHealthDefinition(hp)
      case "Minion" => MinionHealthDefinition
    }
  }

  private def loadRoster(rosterNode: Node): Roster[Combatant] = {
    def loadCombatantNode(combId: CombatantID, normAlias: String, combatantNode: Node): Combatant = {
      val c = Combatant(CombatantRosterDefinition(combId, normAlias,
        parseCombatantEntity(firstMatchingChild(combatantNode, "entity"))))
      val healthDeltaNode = firstMatchingChild(combatantNode, "health-delta")
      c.copy(
        comment = getOptionalTextNode(combatantNode, "comment"),
        health = c.health.applyDelta(loadHealthDelta(healthDeltaNode)),
        effects = EffectList(combId, loadEffects(combId, combatantNode \ "effect"))
      )
    }

    def loadRosterEntry(combatantNode: Node): (CombatantID, Combatant) = {
      val combId = findCombatantID(combatantNode, "@id")
      val normAlias = getOptionalTextNode(combatantNode, "@alias")
      combId -> loadCombatantNode(combId, normAlias, combatantNode)
    }

    def loadHealthDelta(healthDeltaNode: Node): HealthTrackerDelta = {
      HealthTrackerDelta(
        findTagInNodeAsInt(healthDeltaNode, "@damage"),
        findTagInNodeAsInt(healthDeltaNode, "@temporary"),
        findTagInNodeAsInt(healthDeltaNode, "@death-strikes"))
    }

    def loadEffects(combId: CombatantID, nodes: NodeSeq): List[Effect] = {
      def extractCondition(conditionNode: Node): Condition = {
        conditionNode.label match {
          case "generic" =>
            Condition.Generic(conditionNode.child.text, (conditionNode \ "@beneficial" text) == "true")
          case "mark" =>
            Condition.Mark(findCombatantID(conditionNode, "@by"), (conditionNode \ "@permanent" text) == "true")
        }
      }

      def loadDuration(effectNode: Node): Duration = {
        firstMatchingChildOption(effectNode, "@duration") match {
          case Some(durationAttribute) =>
            Duration.staticDurationFromDescription(durationAttribute.text).get
          case None =>
            extractRoundBoundDurationFromNode(effectNode)
        }
      }

      def extractRoundBoundDurationFromNode(effectNode: Node): Duration.RoundBound = {
        RoundBound(
          extractInitiativeOrderID(effectNode, "@limit-order-id"),
          Duration.Limit.withName(getOptionalTextNode(effectNode, "@limit")))
      }

      def extractEffect(effectNode: Node): Effect = {
        Effect(
          EffectID(combId, findTagInNodeAsInt(effectNode, "@effect-number")),
          findCombatantID(effectNode, "@source"),
          extractCondition(effectNode.child(0)),
          loadDuration(effectNode))
      }

      nodes.map(extractEffect).toList
    }

    val rosterMap: Map[CombatantID, Combatant] = Map(rosterNode \ "combatant" map (loadRosterEntry): _*)
    Roster[Combatant](Combatant.RosterFactory, rosterMap)
  }

  private def parseCombatantEntity(combatantNode: Node): CombatantEntity = {
    CombatantEntity(combatantNode \ "entity-id" text,
      combatantNode \ "name" text,
      parseHealthDefinition(firstMatchingChild(combatantNode, "health-definition")),
      findTagInNodeAsInt(combatantNode, "initiative"),
      combatantNode \ "statblock" text)
  }

  private def serializeOrder(order: InitiativeOrder): Node = {
    createSequenceNode("initiative-order", lineBreak ++
      indentNodes(tabStop2,
        serializeOrderEntries(order.baseList, order.tracker) ++
          serializeReorder(order.reorderList) ++
          serializeNextUp(order))
    )
  }

  private def serializeOrderEntries(list: List[InitiativeResult], tracker: Map[InitiativeOrderID, InitiativeTracker]): Seq[Node] = {
    def serializeOrderEntry(result: InitiativeResult, tracker: InitiativeTracker): Elem = {
        <order-entry order-id={result.uniqueId.toXMLNotation} result={result.result.toString}
                     tie-breaker={result.tieBreaker.toString} bonus={result.bonus.toString}
                     round-number={tracker.round.toString} state={tracker.state.toString}/>
    }
    list.map(result => serializeOrderEntry(result, tracker(result.uniqueId))
    )
  }

  private def serializeNextUp(order: InitiativeOrder): Seq[Node] = {
    order.nextUp.map(ioi => <next-up order-id={ioi.toXMLNotation}/>).toSeq
  }

  private def serializeReorder(reorderList: List[(InitiativeOrderID, InitiativeOrderID)]): Seq[Node] = {
    for ((who, whom) <- reorderList) yield {
        <reorder who={who.toXMLNotation}
                 whom={whom.toXMLNotation}/>
    }
  }

  private def loadOrder(node: Node): InitiativeOrder = {
    val baseList = loadBaseList(node \ "order-entry")
    val trackers = Map(baseList.map(entry => entry._2.orderID -> entry._2): _*)
    val comparator = new ReorderedListBuilderCompare[InitiativeResult] {
      def isBefore(a: InitiativeResult, b: InitiativeResult): Boolean = a > b
    }
    val listBuilder = new ReorderedListBuilder(baseList.map(_._1), loadReorders(node \ "reorder"), comparator)
    InitiativeOrder.empty().copy(
      baseList = listBuilder.baseList(),
      sequence = listBuilder.reorderedList(),
      tracker = trackers,
      nextUp = loadNextUp(node),
      reorderList = listBuilder.reorders()
    )
  }

  private def loadBaseList(results: NodeSeq): List[(InitiativeResult, InitiativeTracker)] = {
    def loadInitiativeResult(node: Node, orderId: InitiativeOrderID, totalInitiative: Int): InitiativeResult = {
      InitiativeResult(
        orderId,
        findTagInNodeAsInt(node, "@bonus"),
        totalInitiative,
        findTagInNodeAsInt(node, "@tie-breaker"))
    }

    def loadInitiativeTracker(node: Node, orderId: InitiativeOrderID, totalInitiative: Int): InitiativeTracker = {
      InitiativeTracker(orderId, findTagInNodeAsInt(node, "@round-number"), totalInitiative, InitiativeState.withName(node \ "@state" text))
    }

    def loadResultAndTracker(node: Node): (InitiativeResult, InitiativeTracker) = {
      val orderId = extractInitiativeOrderID(node)
      val totalInitiative = findTagInNodeAsInt(node, "@result")
      (loadInitiativeResult(node, orderId, totalInitiative),
        loadInitiativeTracker(node, orderId, totalInitiative))
    }

    results.map(loadResultAndTracker).toList
  }

  private def loadReorders(reorderNodes: NodeSeq): List[(InitiativeOrderID, InitiativeOrderID)] = {
    reorderNodes.map {
      node => (extractInitiativeOrderID(node, "@who"), extractInitiativeOrderID(node, "@whom"))
    }.toList
  }

  private def loadNextUp(node: Node): Option[InitiativeOrderID] = {
    val nextUpNode = firstMatchingChildOption(node, "next-up")
    nextUpNode.map(extractInitiativeOrderID)
  }

  private def indentNodes(indent: Text, nodes: Seq[Node]): Seq[Node] = {
    nodes.flatMap(node => indent ++ node ++ lineBreak)
  }

  private def extractInitiativeOrderID(node: Node): InitiativeOrderID = {
    orderIdFromXMLNotation(node \ "@order-id" text)
  }

  private def extractInitiativeOrderID(node: Node, attribute: String): InitiativeOrderID = {
    orderIdFromXMLNotation(node \ attribute text)
  }

  private def firstMatchingChild(node: Node, tagName: String): Node = (node \ tagName)(0)

  private def firstMatchingChildOption(node: Node, tagName: String): Option[Node] = {
    val ns = (node \ tagName)
    if (ns.isEmpty) None else Some(ns(0))
  }

  private def getOptionalTextNode(baseNode: Node, tagName: String): String = {
    val search = baseNode \ tagName
    if (search.isEmpty) null
    else search.text
  }

  private def findTagInNodeAsInt(node: Node, tagName: String): Int = {
    (node \ tagName).text.toInt
  }

  private def findCombatantID(node: Node, tagName: String): CombatantID = {
    val xmlNotation = (node \ tagName text)
    CombatantID.fromXMLNotation(xmlNotation).getOrElse {
      throw new Exception("Invalid CombatantID: " + xmlNotation)
    }
  }

  private def createSimpleDataNode(label: String, text: String): Elem = {
    Elem(null, label, Null, TopScope, true, Text(text))
  }

  private def createSequenceNode(label: String, nodes: Seq[Node]): Elem = {
    Elem(null, label, Null, TopScope, true, nodes: _*)
  }

  private def attributeOrNull(identifier: String, value: String): MetaData = {
    if (value != null)
      Attribute(identifier, Text(value), Null)
    else
      Null
  }

  private def orderIdFromXMLNotation(notation: String): InitiativeOrderID = {
    InitiativeOrderID.fromXMLNotation(notation).getOrElse {
      throw new Exception("Invalid InitiativeOrderID: " + notation)
    }
  }
}