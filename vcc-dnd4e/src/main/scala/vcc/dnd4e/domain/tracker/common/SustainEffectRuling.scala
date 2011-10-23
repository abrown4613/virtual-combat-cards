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
package vcc.dnd4e.domain.tracker.common

import vcc.controller.{RulingDecisionHandler, Decision, Ruling}
import vcc.controller.message.TransactionalAction
import vcc.dnd4e.tracker.common.{Command, EffectID}

/**
 * Ruling for the sustain of a given effect.
 * @param eid Effect to sustain
 * @param condition Effect description
 */
case class SustainEffectRuling(eid: EffectID, condition: String) extends Ruling with RulingDecisionHandler[List[TransactionalAction]] {
  protected def decisionValidator(answer: Decision[_]): Boolean = answer match {
    case o: SustainEffectDecision => true
    case _ => false
  }

  def processDecision(decision: Decision[_ <: Ruling]): List[TransactionalAction] = {
    decision match {
      case SustainEffectDecision(q, SustainEffectDecision.Sustain) => List(Command.SustainEffect(q.eid))
      case _ => Nil
    }
  }
}

/**
 * Decision for a SustainEffectRuling
 * @param ruling Ruling on which this decision applies
 * @param result On of the SustainEffectDecision values
 */
case class SustainEffectDecision(ruling: SustainEffectRuling, result: SustainEffectDecision.Value) extends Decision[SustainEffectRuling]

/**
 * SustainEffectDecision companion which defines possible outcomes of a sustain ruling.
 */
object SustainEffectDecision extends Enumeration {
  val Sustain = Value("Sustain effect")
  val Cancel = Value("Cancel effect")
}