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
 * For effect that degenerate into worst things
 * @param eid EffectID of the effect to chance
 * @param condition Current condition. If  the format include arrows (->) or slash (/) this effect will be reprocessed
 * prior to display. This format is used to indicate de notion of progression.
 */
case class SaveEffectSpecialRuling(eid: EffectID, condition: String) extends Ruling with RulingDecisionHandler[List[TransactionalAction]] {
  def decisionValidator(decision: Decision[_]): Boolean = decision match {
    case SaveEffectSpecialDecision(q, _) => true
    case _ => false
  }

  def processDecision(decision: Decision[_ <: Ruling]): List[TransactionalAction] = {
    decision match {
      case SaveEffectSpecialDecision(q, SaveEffectSpecialDecision.Saved) =>
        List(CancelEffect(q.eid))
      case SaveEffectSpecialDecision(q, SaveEffectSpecialDecision.Changed(newEffect)) =>
        List(UpdateEffectCondition(q.eid, Effect.Condition.Generic(newEffect, false)))
      case _ => Nil
    }
  }
}


object SaveEffectSpecialDecision {

  sealed trait Result

  case object Saved extends Result

  case class Changed(newCondition: String) extends Result

}

/**
 * Decision for a SaveEffectSpecialRuling.
 * @param ruling Referred ruling
 * @param result Result can be one of Saved or Changed(n) where n is the new condition to be applied.
 */
case class SaveEffectSpecialDecision(ruling: SaveEffectSpecialRuling, result: SaveEffectSpecialDecision.Result) extends Decision[SaveEffectSpecialRuling]

