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
import vcc.dnd4e.tracker.ruling.OngoingDamage.DamageToApply
import vcc.tracker.{StateCommand, Question, Ruling}
import vcc.dnd4e.tracker.transition.DamageCommand

object OngoingDamage {

  case class CausedBy(eid: EffectID) extends Question[CombatState] {
    def userPrompt(state: CombatState): String = {
      val comb = state.roster.combatant(eid.combId)
      val effect = comb.effects.find(eid).get
      comb.name + " " + eid.combId.simpleNotation + " affected by: " + effect.condition.description
    }
  }

  case class DamageToApply(amount: Int)
}

case class OngoingDamageRuling(question: OngoingDamage.CausedBy, decision: Option[OngoingDamage.DamageToApply])
  extends Ruling[CombatState, OngoingDamage.CausedBy, OngoingDamage.DamageToApply, OngoingDamageRuling] {
  protected def commandsFromDecision(state: CombatState): List[StateCommand[CombatState]] = {
    decision.get match {
      case DamageToApply(amount) if (amount > 0) => DamageCommand(question.eid.combId, amount) :: Nil
      case _ => Nil
    }
  }

  def withDecision(decision: DamageToApply): OngoingDamageRuling = copy(decision = Option(decision))
}