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
import vcc.dnd4e.tracker.command.DamageCommand
import vcc.tracker.{Command, Ruling}

case class OngoingDamageRuling(sourceEffect: EffectID, decision: Option[Int])
  extends Ruling[CombatState, Int, OngoingDamageRuling] {

  def isRulingSameSubject(otherRuling: Ruling[CombatState, _, _]): Boolean = {
    otherRuling.asInstanceOf[AnyRef] match {
      case OngoingDamageRuling(otherSourceEffect,_) => otherSourceEffect == this.sourceEffect
      case _ => false
    }
  }

  def userPrompt(state: CombatState): String = {
    val eid = sourceEffect
    val comb = state.roster.combatant(eid.combId)
    val effect = comb.effects.find(eid).get
    comb.name + " " + eid.combId.simpleNotation + " affected by: " + effect.condition.description
  }

  protected def commandsFromDecision(state: CombatState): List[Command[CombatState]] = {
    val amount = decision.get
    if(amount > 0 )
      DamageCommand(sourceEffect.combId, amount) :: Nil
    else
      Nil
  }

  def withDecision(decision: Int): OngoingDamageRuling = copy(decision = Some(decision))
}