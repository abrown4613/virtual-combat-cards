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
import vcc.dnd4e.tracker.common.Effect.Condition

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
  val bigStateCommands = Seq(
    AddCombatantCommand(Some(combA), null, entityPc1),
    AddCombatantCommand(Some(combB), null, entityPc2),
    AddCombatantCommand(None, null, entityMonster),
    AddCombatantCommand(None, null, entityMinion),
    SetInitiativeCommand(InitiativeDefinition(combA, 5, List(10))),
    SetInitiativeCommand(InitiativeDefinition(combB, 1, List(5))),
    SetInitiativeCommand(InitiativeDefinition(comb1, 2, List(7))),
    StartCombatCommand,
    DamageCommand(combB, 10),
    SetTemporaryHPCommand(combB, 6)
  )

  val singleCharacterInOrder = Seq(
    AddCombatantCommand(Some(combA), null, entityPc1),
    SetInitiativeCommand(InitiativeDefinition(combA, 5, List(10))),
    StartCombatCommand)

  val bigStateWithDead = bigStateCommands ++ Seq(
    DamageCommand(combB, 100),
    DamageCommand(comb1, 100)
  )

  val stateWithEffect = bigStateCommands ++ Seq(
    AddEffectCommand(comb1, comb1, Condition.Generic("Regen 10", true), Duration.EndOfEncounter),
    AddEffectCommand(comb1, combA, Condition.Generic("Slowed", false), Duration.RoundBound(ioA0, Duration.Limit.EndOfNextTurn)),
    AddEffectCommand(combA, combA, Condition.Generic("Regen 2", true), Duration.Stance),
    AddEffectCommand(combA, comb1, Condition.Generic("Immobilized", false), Duration.SaveEnd)
  )

  val finalState = buildState(bigStateCommands)

  def is =
    "StateFormatter".title ^
      "format single pc correctly" ! withCommands(singleCharacterInOrder).hasCompleteFighterAt(0) ^
      "format monster correctly" ! withCommands(bigStateCommands).hasCompleteMonsterAt(1) ^
      "format second pc correctly" ! withCommands(bigStateCommands).hasCompleteMageAt(2) ^
      "format empty state as an empty array" ! emptyFormat() ^
      "don't show creatures that are not in the order" ! withCommands(bigStateCommands).doesNotShowCombatantOutOfOrder() ^
      "show dead combatants" ! withCommands(bigStateWithDead).hideDead() ^
      "show all effects for character" ! withCommands(stateWithEffect).showAllOnCharacter() ^
      "show only character generated for monster" ! withCommands(stateWithEffect).showMonsterEffects()

  case class withCommands(commands: Seq[CombatStateCommand]) {
    val format = new StateFormatter().format(buildState(commands))

    def hasCompleteFighterAt(position: Int) = {
      (format must /#(position) / ("id" -> "Aº")) and (format must /#(position) / ("name" -> "Fighter")) and
        (format must /#(position) / ("health" -> "40 / 40")) and (format must /#(position) / ("status" -> "Ok")) and
        (format must /#(position) / ("isCharacter" -> true))
    }

    def hasCompleteMonsterAt(position: Int) = {
      (format must /#(position) / ("id" -> "1º")) and (format must /#(position) / ("name" -> "Monster")) and
        (format must not(/#(position) / ("health" -> ".*".r))) and (format must /#(position) / ("status" -> "Ok")) and
        (format must not(/#(position) / ("isCharacter" -> ".*".r)))
    }

    def hasCompleteMageAt(position: Int) = {
      (format must /#(position) / ("id" -> "Bº")) and (format must /#(position) / ("name" -> "Mage")) and
        (format must /#(position) / ("health" -> "15 / 25 +6")) and (format must /#(position) / ("status" -> "Ok"))
    }

    def doesNotShowCombatantOutOfOrder() = {
      (format must not */ ("id" -> "2"))
    }

    def hideDead() = {
      (format must not */ ("id" -> "1º")) and (format must not */ ("id" -> "Bº"))
    }

    def showAllOnCharacter() = {
      matchEffect(0, 0, "Immobilized", "SE") and
        matchEffect(0, 1, "Regen 2", "Stance") and
        (format must not(/#(2) / "effects"))
    }

    def showMonsterEffects() = {
      matchEffect(1, 0, "Slowed", "EoNT:Aº") and
        matchEffect(1, 1, "Regen 10", "EoE").negate
    }

    def matchEffect(pos: Int, effectIndex: Int, description: String, duration: String) = {
      (format must /#(pos) / ("effects") /# (effectIndex) / ("description" -> description)) and
        (format must /#(pos) / ("effects") /# (effectIndex) / ("duration" -> duration))
    }
  }

  private def emptyFormat() = {
    val fmt = new StateFormatter()
    fmt.format(CombatState.empty) must_== "[]"
  }

  private def buildState(commands: Seq[CombatStateCommand]): CombatState = {
    commands.foldLeft(CombatState.empty)((ns, cmd) =>
      ns.transitionWith(cmd.generateEvents(ns)))
  }
}