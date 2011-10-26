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

import vcc.tracker.{StateCommand, Question, Ruling}
import vcc.dnd4e.tracker.transition.{SustainEffectCommand}
import vcc.dnd4e.tracker.common.{Effect, EffectID, CombatState}

object SustainEffect {

  sealed trait Result

  case object Sustain extends Result

  case object Cancel extends Result

  case class ToSustain(eid: EffectID) extends Question[CombatState] {
    def userPrompt(state: CombatState): String = "Sustain effect " + state.roster.combatant(eid.combId).effects.effects.find(x => x.effectId == eid).map(_.condition.description).getOrElse("")
  }

  def fromEffect(effect: Effect): SustainEffectRuling = {
    SustainEffectRuling(ToSustain(effect.effectId), None)
  }
}

case class SustainEffectRuling(question: SustainEffect.ToSustain, decision: Option[SustainEffect.Result])
  extends Ruling[CombatState, SustainEffect.ToSustain, SustainEffect.Result, SustainEffectRuling] {

  import SustainEffect._

  protected def commandsFromDecision(state: CombatState): List[StateCommand[CombatState]] = {
    decision.get match {
      case Cancel => Nil
      case Sustain => List(SustainEffectCommand(question.eid))
    }
  }

  def withDecision(value: Result): SustainEffectRuling = copy(decision = Option(value))
}