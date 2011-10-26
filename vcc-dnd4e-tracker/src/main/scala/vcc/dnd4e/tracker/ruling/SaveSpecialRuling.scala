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
package vcc.dnd4e.tracker.ruling

import vcc.tracker.{StateCommand, Ruling, Question}
import vcc.dnd4e.tracker.transition.CancelEffectCommand
import vcc.dnd4e.tracker.transition.UpdateEffectConditionCommand
import vcc.dnd4e.tracker.common.{Effect, CombatState, EffectID}

object SaveSpecial {

  sealed trait Result

  case class Changed(newCondition: String) extends Result

  case object Saved extends Result

  //TODO This is duplicate for make
  case class Against(eid: EffectID, what: String) extends Question[CombatState] {
    def userPrompt(state: CombatState): String = {
      state.roster.combatant(eid.combId).name + " must make a saving throws against " + what
    }
  }

  def rulingFromEffect(effect:Effect): SaveSpecialRuling = {
    SaveSpecialRuling(Against(effect.effectId, effect.condition.description),None)
  }
}

case class SaveSpecialRuling(question: SaveSpecial.Against, decision: Option[SaveSpecial.Result])
  extends Ruling[CombatState, SaveSpecial.Against, SaveSpecial.Result, SaveSpecialRuling] {

  import SaveSpecial._
  protected def commandsFromDecision(state: CombatState): List[StateCommand[CombatState]] = {
    decision.get match {
      case Saved => List(CancelEffectCommand(question.eid))
      case Changed(newCondition)=> List(UpdateEffectConditionCommand(question.eid, Effect.Condition.Generic(newCondition, false)))
    }
  }

  def withDecision(decision: Result): SaveSpecialRuling = copy(decision = Some(decision))
}