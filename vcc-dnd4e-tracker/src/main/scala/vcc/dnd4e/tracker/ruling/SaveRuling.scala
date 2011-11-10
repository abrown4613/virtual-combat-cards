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
import vcc.tracker.{StateCommand, Ruling}

object SaveRulingResult extends Enumeration {
  val Saved = Value("Saved")
  val Failed = Value("Failed")
}

case class SaveRuling(sourceEffect: EffectID, decision: Option[SaveRulingResult.Value])
  extends Ruling[CombatState, SaveRulingResult.Value, SaveRuling] {

  import SaveRulingResult._

  def isRulingSameSubject(otherRuling: Ruling[CombatState, _, _]): Boolean = {
    otherRuling match {
      case SaveRuling(otherSourceEffect, _) => this.sourceEffect == otherSourceEffect
      case _ => false
    }
  }

  def userPrompt(state: CombatState): String = {
    val eid = sourceEffect
    val combatant = state.combatant(eid.combId)
    combatant.name + " " + eid.combId.simpleNotation + " must make a saving throws against: " + combatant.effects.find(eid).get.condition.description
  }

  protected def commandsFromDecision(state: CombatState): List[StateCommand[CombatState]] = {
    decision.get match {
      case Saved => List(CancelEffectCommand(sourceEffect))
      case Failed => Nil
    }
  }

  def withDecision(value: Value): SaveRuling = copy(decision = Some(value))
}






