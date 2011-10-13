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

import vcc.dnd4e.tracker.common.{EffectID, CombatState}
import vcc.dnd4e.tracker.transition.{CancelEffectCommand}
import vcc.tracker.{Question, StateCommand, Ruling}

object Save {

  case class Against(eid: EffectID, what: String) extends Question[CombatState] {
    def userPrompt(state: CombatState): String = {
      state.roster.combatant(eid.combId).definition.entity.name + " must make a saving throws against " + what
    }
  }

  sealed trait Result

  case object Saved extends Result

  case object Failed extends Result

}

case class SaveRuling(question: Save.Against, decision: Option[Save.Result]) extends Ruling[CombatState, Save.Against, Save.Result, SaveRuling] {
  import Save._


  protected def commandsFromDecision(state: CombatState): List[StateCommand[CombatState]] = {
    decision.get match {
      case Saved => List(CancelEffectCommand(question.eid))
      case Failed => Nil
    }
  }

  def withDecision(value: Result): SaveRuling = copy(decision = Some(value))
}






