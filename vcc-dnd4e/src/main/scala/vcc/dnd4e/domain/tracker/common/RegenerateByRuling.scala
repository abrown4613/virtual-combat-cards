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

import vcc.controller.{RulingDecisionHandler, Ruling, Decision}
import vcc.controller.message.TransactionalAction
import vcc.dnd4e.tracker.common.{Command, EffectID}

/**
 * Ask how much damage should be healed.
 * @param eid Effect that produces the regenerations
 * @param description Full description of the effect/condition.
 * @param hintValue Recommended amount of regeneration (extracted from condition).
 */
case class RegenerateByRuling(eid: EffectID, description: String, hintValue: Int) extends Ruling with RulingDecisionHandler[List[TransactionalAction]] {
  protected def decisionValidator(answer: Decision[_]): Boolean = answer match {
    case a: RegenerateByDecision => true
    case _ => false
  }

  def processDecision(decision: Decision[_ <: Ruling]): List[TransactionalAction] = {
    decision match {
      case RegenerateByDecision(r, value) if (value > 0) => List(Command.HealDamage(r.eid.combId, value))
      case _ => Nil
    }
  }
}

/**
 * Amount that should be healed by this decision
 */
case class RegenerateByDecision(ruling: RegenerateByRuling, amount: Int) extends Decision[RegenerateByRuling]
