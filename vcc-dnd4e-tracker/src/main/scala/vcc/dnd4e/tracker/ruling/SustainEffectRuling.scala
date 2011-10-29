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

import vcc.tracker.{StateCommand, Ruling}
import vcc.dnd4e.tracker.transition.{SustainEffectCommand}
import vcc.dnd4e.tracker.common.{EffectID, CombatState}

object SustainEffectRulingResult extends Enumeration {
  val Sustain = Value("Sustain")
  val Cancel = Value("Cancel")
}

case class SustainEffectRuling(sustainableEffect: EffectID, decision: Option[SustainEffectRulingResult.Value])
  extends Ruling[CombatState, SustainEffectRulingResult.Value, SustainEffectRuling] {

  import SustainEffectRulingResult._

  def isRulingSameSubject(otherRuling: Ruling[CombatState, _, _]): Boolean = {
    otherRuling match {
      case SustainEffectRuling(otherSustainableEffect, _) => otherSustainableEffect == this.sustainableEffect
      case _ => false
    }
  }

  def userPrompt(state: CombatState) = {
    val eid = sustainableEffect
    val combatant = state.combatant(eid.combId)
    val effect = combatant.effects.find(eid).get
    "Sustain \"" + effect.condition.description + "\" (from " + combatant.name + " " + eid.combId.simpleNotation + ")"
  }

  protected def commandsFromDecision(state: CombatState): List[StateCommand[CombatState]] = {
    decision.get match {
      case Cancel => Nil
      case Sustain => List(SustainEffectCommand(sustainableEffect))
    }
  }

  def withDecision(value: SustainEffectRulingResult.Value): SustainEffectRuling = copy(decision = Option(value))
}