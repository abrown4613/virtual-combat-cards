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
package vcc.dnd4e.web.util

import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.command._
import vcc.dnd4e.tracker.common.MonsterHealthDefinition
import vcc.dnd4e.tracker.common.CombatantEntity
import vcc.dnd4e.tracker.common.CharacterHealthDefinition
import scala.Some
import vcc.dnd4e.tracker.command.AddCombatantCommand
import org.specs2.SpecificationWithJUnit
import util.parsing.json.{JSONArray, Parser, JSONType}

trait SampleStateData {
  val combA = CombatantID("A")
  val ioA0 = InitiativeOrderID(combA, 0)
  val ioA1 = InitiativeOrderID(combA, 1)

  val combB = CombatantID("B")
  val ioB0 = InitiativeOrderID(combB, 0)

  val comb1 = CombatantID("1")
  val io1_0 = InitiativeOrderID(comb1, 0)

  val comb2 = CombatantID("2")
  val io2_0 = InitiativeOrderID(comb2, 0)

  //Val CombatantEntity
  val entityPc1 = CombatantEntity(null, "Fighter", CharacterHealthDefinition(40), 2, null)
  val entityPc2 = CombatantEntity(null, "Mage", CharacterHealthDefinition(25), 4, null)
  val entityMinion = CombatantEntity(null, "Minion", MinionHealthDefinition, 1, null)
  val entityMonster = CombatantEntity(null, "Monster", MonsterHealthDefinition(30), 1, null)
}

class StateFormatterTest extends SpecificationWithJUnit with SampleStateData {
  val stateBuildingCommands = Seq(
    AddCombatantCommand(Some(combA), null, entityPc1),
    AddCombatantCommand(Some(combB), null, entityPc2),
    AddCombatantCommand(None, null, entityMonster),
    AddCombatantCommand(None, null, entityMinion),
    SetInitiativeCommand(InitiativeDefinition(combA, 5, List(10))),
    SetInitiativeCommand(InitiativeDefinition(combA, 1, List(5))),
    SetInitiativeCommand(InitiativeDefinition(comb1, 2, List(7))),
    StartCombatCommand
  )

  val singleCharacterInOrder = Seq(
    AddCombatantCommand(Some(combA), null, entityPc1),
    SetInitiativeCommand(InitiativeDefinition(combA, 5, List(10))),
    StartCombatCommand)

  val finalState = buildState(stateBuildingCommands)


  def is =
    "StateFormatter" ^
      "format single pc correctly" ! withCommands(singleCharacterInOrder).hasCompleteFighterAt(0) ^
      "format single monster correctly" ! withCommands(stateBuildingCommands).hasCompleteMonsterAt(0) ^
      "format empty state as an empty array" ! emptyFormat()

  case class withCommands(commands: Seq[CombatStateCommand]) {
    val format = new StateFormatter().format(buildState(commands))

    def hasCompleteFighterAt(position: Int) = {
      val element = nthArray(format, position)
      (element must /("id" -> "Aº")) and (element must /("name" -> "Fighter")) and
        (element must /("health" -> "40 / 40")) and (element must /("status" -> "Ok"))
    }

    def hasCompleteMonsterAt(position: Int) = {
      val element = nthArray(format, position)
      (element must /("id" -> "1º")) and (element must /("name" -> "Monster")) and
        (element must not / ("health" -> ".*".r)) and (element must /("status" -> "Ok"))
    }
  }

  def emptyFormat() = {
    val fmt = new StateFormatter()
    fmt.format(CombatState.empty) must_== "[]"
  }

  private def buildState(commands: Seq[CombatStateCommand]): CombatState = {
    commands.foldLeft(CombatState.empty)((ns, cmd) =>
      ns.transitionWith(cmd.generateEvents(ns)))
  }

  private def nthArray(input: String, index: Int): String = {
    parse(input) match {
      case Some(JSONArray(array)) => array(index).toString
      case _ => throw new NoSuchElementException("Not a JSON array")
    }
  }

  private def parse(s: String): Option[JSONType] =
    new Parser {
      def parseRaw(input: String): Option[JSONType] =
        phrase(root)(new lexical.Scanner(input)) match {
          case Success(result, _) => Some(result)
          case _ => None
        }
    }.parseRaw(s)

}