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

import vcc.dnd4e.tracker.common.{CombatState, EffectID}
import vcc.tracker.{StateCommand, Ruling, Question}
import vcc.dnd4e.tracker.transition.HealCommand

case class CausedBy(eid: EffectID) extends Question[CombatState] {
  def userPrompt(state: CombatState): String = {
    val comb = state.roster.combatant(eid.combId)
    val effect = comb.effects.find(eid).get
    comb.name + " " + eid.combId.simpleNotation + " affected by: " + effect.condition.description
  }
}

case class RegenerationRuling(question: CausedBy, decision: Option[Int])
  extends Ruling[CombatState, CausedBy, Int, RegenerationRuling] {

  protected def commandsFromDecision(state: CombatState): List[StateCommand[CombatState]] = {
    if(decision.get > 0) HealCommand(question.eid.combId, decision.get) :: Nil
    else Nil
  }

  def withDecision(decision: Int): RegenerationRuling = copy(decision = Some(decision))
}