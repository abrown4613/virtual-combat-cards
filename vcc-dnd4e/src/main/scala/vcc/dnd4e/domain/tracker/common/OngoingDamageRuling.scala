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
//$Id$
package vcc.dnd4e.domain.tracker.common

import vcc.controller.{RulingDecisionHandler, Ruling, Decision}
import vcc.controller.message.TransactionalAction

/**
 * Should ongoing damage be applied?
 * @param eid Effect causing the ongoing damage
 * @param description Text of the ongoing effect
 * @param hintValue Initial value
 */
case class OngoingDamageRuling(eid: EffectID, description: String, hintValue: Int) extends Ruling with RulingDecisionHandler[List[TransactionalAction]] {
  protected def decisionValidator(answer: Decision[_]): Boolean = answer match {
    case a: OngoingDamageDecision => true
    case _ => false
  }

  def processDecision(decision: Decision[_ <: Ruling]): List[TransactionalAction] = {
    decision match {
      case OngoingDamageDecision(r, value) if (value > 0) => List(Command.ApplyDamage(r.eid.combId, value))
      case _ => Nil
    }
  }
}

/**
 * Amount of ongoing damage to apply
 * @param ruling Refers to ruling
 * @param amount Amount of damage to apply
 */
case class OngoingDamageDecision(ruling: OngoingDamageRuling, amount: Int) extends Decision[OngoingDamageRuling]

