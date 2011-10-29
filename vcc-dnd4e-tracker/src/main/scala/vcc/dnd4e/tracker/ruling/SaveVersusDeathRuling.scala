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
import vcc.dnd4e.tracker.ruling.SaveVersusDeath.Result
import vcc.tracker.{StateCommand, Question, Ruling}
import vcc.dnd4e.tracker.transition.{FailDeathSaveCommand, HealCommand}

object SaveVersusDeath {

  case class Dying(cid: CombatantID) extends Question[CombatState] {
    def userPrompt(state: CombatState): String = null
  }

  object Result extends Enumeration {
    val Saved = Value("Saved")
    val Failed = Value("Failed")
    val SaveAndHeal = Value("Save and heal 1 hp")
  }

  def createRuling(cid: CombatantID): SaveVersusDeathRuling = SaveVersusDeathRuling(Dying(cid), None)
}

case class SaveVersusDeathRuling(question: SaveVersusDeath.Dying, decision: Option[SaveVersusDeath.Result.Value])
  extends Ruling[CombatState, SaveVersusDeath.Dying, SaveVersusDeath.Result.Value, SaveVersusDeathRuling] {


  def userPrompt(state: CombatState) = {
    val combatant = state.combatant(question.cid)
    "Save versus death for " + combatant.name + " " + question.cid.simpleNotation
  }

  protected def commandsFromDecision(state: CombatState): List[StateCommand[CombatState]] = {
    import SaveVersusDeath.Result._
    decision.get match {
      case Saved => Nil
      case SaveAndHeal => List(HealCommand(question.cid, 1))
      case Failed => List(FailDeathSaveCommand(question.cid))
    }
  }

  def withDecision(decision: Result.Value): SaveVersusDeathRuling = copy(decision = Some(decision))
}