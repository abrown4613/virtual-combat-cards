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

import vcc.dnd4e.tracker.common.{CombatState, CombatantID}
import vcc.tracker.{StateCommand, Ruling}
import vcc.dnd4e.tracker.transition.{FailDeathSaveCommand, HealCommand}

object SaveVersusDeathResult extends Enumeration {
  val Saved = Value("Saved")
  val Failed = Value("Failed")
  val SaveAndHeal = Value("Save and heal 1 hp")
}

case class SaveVersusDeathRuling(sourceEffect: CombatantID, decision: Option[SaveVersusDeathResult.Value])
  extends Ruling[CombatState, SaveVersusDeathResult.Value, SaveVersusDeathRuling] {

  def isRulingSameSubject(otherRuling: Ruling[CombatState, _, _]): Boolean = {
    otherRuling match {
      case SaveVersusDeathRuling(otherSourceEffect, _) => otherSourceEffect == this.sourceEffect
      case _ => false
    }
  }

  def userPrompt(state: CombatState) = {
    val combatant = state.combatant(sourceEffect)
    "Save versus death for " + combatant.name + " " + sourceEffect.simpleNotation
  }

  protected def commandsFromDecision(state: CombatState): List[StateCommand[CombatState]] = {
    import SaveVersusDeathResult._
    decision.get match {
      case Saved => Nil
      case SaveAndHeal => List(HealCommand(sourceEffect, 1))
      case Failed => List(FailDeathSaveCommand(sourceEffect))
    }
  }

  def withDecision(decision: SaveVersusDeathResult.Value): SaveVersusDeathRuling = copy(decision = Some(decision))
}
