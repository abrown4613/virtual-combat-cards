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

import vcc.dnd4e.tracker.common.{InitiativeOrderID, CombatState}
import vcc.dnd4e.tracker.transition.{MoveUpCommand, StartRoundCommand}
import vcc.tracker.{IllegalDecisionException, StateCommand, Question, Ruling}

case class EligibleNext(primary: InitiativeOrderID, other: List[InitiativeOrderID]) extends Question[CombatState] {
  def userPrompt(state: CombatState): String = null
}

case class NextUpRuling(question: EligibleNext, decision: Option[InitiativeOrderID]) extends Ruling[CombatState, EligibleNext, InitiativeOrderID, NextUpRuling] {


  def userPrompt(state: CombatState):String = "Select which combatant should act next"

  protected def commandsFromDecision(state: CombatState): List[StateCommand[CombatState]] = {
    val ioi = decision.get
    (if (ioi == question.primary) StartRoundCommand(ioi) else MoveUpCommand(ioi)) :: Nil
  }

  def withDecision(value: InitiativeOrderID): NextUpRuling = {
    if (value != question.primary && !question.other.contains(value))
      throw new IllegalDecisionException(value + " is not eligible to act")
    copy(decision = Some(value))
  }
}