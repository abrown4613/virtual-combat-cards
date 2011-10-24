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
package vcc.dnd4e.tracker.dispatcher

import org.specs2.SpecificationWithJUnit
import vcc.controller.message.TransactionalAction
import vcc.dnd4e.tracker.common.Command._
import vcc.dnd4e.tracker.transition._
import vcc.dnd4e.tracker.transition.AutomationCommandSource._
import vcc.tracker.{SeqCommandStream, StateCommand}
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.common.Effect.Condition

class ActionTranslatorTest extends SpecificationWithJUnit with SampleStateData {
  private val eid = EffectID(combA, 0)
  private val someCondition = Condition.Generic("good", true)
  private val someDuration = Duration.EndOfEncounter
  private val rd1 = CombatantRosterDefinition(null, "alias", entityMonster)
  private val rd2 = CombatantRosterDefinition(combA, null, entityPc1)
  private val iDef1 = InitiativeDefinition(combA, 0, List(10))
  private val iDef2 = InitiativeDefinition(comb1, 1, List(11))

  private case class SimpleCase(action: TransactionalAction, commands: StateCommand[CombatState]*) {
    def getTestName = (action.getClass.getSimpleName + " to " + commands.map(x => x.getClass.getSimpleName).mkString(", "))
  }

  private val baseCases: List[SimpleCase] = List(
    SimpleCase(AddCombatants(List(rd1, rd2)), AddCombatantCommand(None, "alias", entityMonster), AddCombatantCommand(Some(combA), null, entityPc1)),
    SimpleCase(SetInitiative(List(iDef1, iDef2)), SetInitiativeCommand(iDef1), SetInitiativeCommand(iDef2)),
    SimpleCase(SetComment(combA, "text"), SetCombatantCommentCommand(combA, "text")),
    SimpleCase(SetCombatComment("comment"), SetCombatCommentCommand("comment")),
    SimpleCase(ApplyRest(true), RestCommand(true)),
    SimpleCase(EndCombat(), EndCombatCommand),
    SimpleCase(ClearRoster(true), ClearRosterCommand(false)) //TODO Improve semantic change
  )

  private val healthCases = List(
    SimpleCase(ApplyDamage(combA, 10), DamageCommand(combA, 10)),
    SimpleCase(HealDamage(combA, 11), HealCommand(combA, 11)),
    SimpleCase(SetTemporaryHP(combA, 10), SetTemporaryHPCommand(combA, 10)),
    SimpleCase(FailDeathSave(combA), FailDeathSaveCommand(combA)),
    SimpleCase(RevertDeath(combA), RevertDeathCommand(combA))
  )

  private val initiativeCases = List(
    SimpleCase(ExecuteInitiativeAction(ioA0, InitiativeAction.StartRound), StartRoundCommand(ioA0)),
    SimpleCase(ExecuteInitiativeAction(ioA0, InitiativeAction.MoveUp), MoveUpCommand(ioA0)),
    SimpleCase(ExecuteInitiativeAction(ioA0, InitiativeAction.ExecuteReady), ExecuteReadyCommand(ioA0))
  )

  private val effectCases = List(
    SimpleCase(AddEffect(combA, combB, someCondition, someDuration), AddEffectCommand(combA, combB, someCondition, someDuration)),
    SimpleCase(UpdateEffectCondition(eid, someCondition), UpdateEffectConditionCommand(eid, someCondition)),
    SimpleCase(CancelEffect(eid), CancelEffectCommand(eid)),
    SimpleCase(SustainEffect(eid), SustainEffectCommand(eid))
  )

  def is = "ActionTranslator".title ^
    "base cases" ^ allSimpleCases(baseCases) ^ endp ^
    "health cases" ^ allSimpleCases(healthCases) ^ endp ^
    "initiative cases" ^ allSimpleCases(initiativeCases) ^ endp ^
    "effect cases" ^ allSimpleCases(effectCases) ^ endp ^
    "CancelEffect" ! directTranslation(CancelEffect(eid), CancelEffectCommand(eid)) ^
    "Complex cases" ^
    "  StartCombat and autostart" ! actionWithAutoStart(StartCombat(), StartCombatCommand) ^
    "  EndRound and autostart" ! actionWithAutoStart(ExecuteInitiativeAction(ioA0, InitiativeAction.EndRound), EndRoundCommand(ioA0)) ^
    "  DelayAction and autostart" ! actionWithAutoStart(ExecuteInitiativeAction(ioA0, InitiativeAction.DelayAction), DelayCommand(ioA0)) ^
    "  ReadyAction and autostart" ! actionWithAutoStart(ExecuteInitiativeAction(ioA0, InitiativeAction.ReadyAction), ReadyActionCommand(ioA0), EndRoundCommand(ioA0)) ^
    endp ^
    end

  private def actionWithAutoStart(action: TransactionalAction, commands: StateCommand[CombatState]*) = {
    ActionTranslator.translateToCommandStream(action) must_== (SeqCommandStream(commands) followedBy autoStartDead followedBy autoStartNext)
  }

  private def allSimpleCases(cases: List[SimpleCase]) = {
    for (c <- cases) yield {
      c.getTestName ! {
        ActionTranslator.translateToCommandStream(c.action) must_== SeqCommandStream(c.commands)
      }
    }
  }

  def directTranslation(action: TransactionalAction, commands: StateCommand[CombatState]*) = {
    ActionTranslator.translateToCommandStream(action) must_== SeqCommandStream(commands)
  }
}