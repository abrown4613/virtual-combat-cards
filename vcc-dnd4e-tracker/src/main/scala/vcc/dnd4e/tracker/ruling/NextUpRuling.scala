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
import vcc.dnd4e.tracker.command.{NextUpCommand, MoveUpCommand, StartRoundCommand}
import vcc.tracker.{Command, InvalidDecisionException, Ruling}

case class NextUpRuling(candidates: NextUpCommand, decision: Option[InitiativeOrderID])
  extends Ruling[CombatState, InitiativeOrderID, NextUpRuling] {

  def isRulingSameSubject(other: Ruling[CombatState, _, _]): Boolean = {
    other match {
      case NextUpRuling(otherCandidates, _) => this.candidates == otherCandidates
      case _ => false
    }
  }

  def userPrompt(state: CombatState):String = "Select which combatant should act next"

  protected def commandsFromDecision(state: CombatState): List[Command[CombatState]] = {
    val ioi = decision.get
    (if (ioi == candidates.next) StartRoundCommand(ioi) else MoveUpCommand(ioi)) :: Nil
  }

  def withDecision(value: InitiativeOrderID): NextUpRuling = {
    if (value != candidates.next && !candidates.delaying.contains(value))
      throw new InvalidDecisionException(value + " is not eligible to act")
    copy(decision = Some(value))
  }
}