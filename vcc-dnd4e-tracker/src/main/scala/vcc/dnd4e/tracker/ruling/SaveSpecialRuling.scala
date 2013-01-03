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

import vcc.dnd4e.tracker.command.CancelEffectCommand
import vcc.dnd4e.tracker.command.UpdateEffectConditionCommand
import vcc.dnd4e.tracker.common.{Effect, CombatState, EffectID}
import vcc.tracker.{Command, Ruling}

sealed trait SaveSpecialRulingResult

object SaveSpecialRulingResult {

  case class Changed(newCondition: String) extends SaveSpecialRulingResult

  case object Saved extends SaveSpecialRulingResult

}

object SaveSpecialRuling {
  def rulingFromEffect(effect: Effect): SaveSpecialRuling = {
    SaveSpecialRuling(effect.effectId, None)
  }
}

case class SaveSpecialRuling(sourceEffect: EffectID, decision: Option[SaveSpecialRulingResult])
  extends Ruling[CombatState, SaveSpecialRulingResult, SaveSpecialRuling] {

  import SaveSpecialRulingResult._

  def isRulingSameSubject(other: Ruling[CombatState, _, _]): Boolean = {
    other.asInstanceOf[AnyRef] match {
      case SaveSpecialRuling(otherSourceEffect, _) => this.sourceEffect == otherSourceEffect
      case _ => false
    }
  }

  def userPrompt(state: CombatState): String = {
    val eid = sourceEffect
    val combatant = state.combatant(eid.combId)
    combatant.name + " " + eid.combId.simpleNotation +
      " must make a saving throws against: " + combatant.effects.find(eid).get.condition.description
  }

  protected def commandsFromDecision(state: CombatState): List[Command[CombatState]] = {
    decision.get match {
      case Saved => List(CancelEffectCommand(sourceEffect))
      case Changed(newCondition) => List(UpdateEffectConditionCommand(sourceEffect, Effect.Condition.Generic(newCondition, false)))
    }
  }

  def withDecision(decision: SaveSpecialRulingResult): SaveSpecialRuling = copy(decision = Some(decision))
}