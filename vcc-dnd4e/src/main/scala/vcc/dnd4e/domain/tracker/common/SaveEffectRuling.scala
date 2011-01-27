/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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

import vcc.controller.message.TransactionalAction
import vcc.controller.{RulingDecisionHandler, Ruling, Decision}
import vcc.dnd4e.domain.tracker.common.Command.{UpdateEffectCondition, CancelEffect}

//SAVE

object SaveEffectRuling {
  def fromEffect(effect: Effect): SaveEffectRuling = {
    effect.duration match {
      case Duration.SaveEnd =>
        SaveEffectRuling(effect.effectId, effect.condition.description)
      case _ => null
    }
  }
}

/**
 * For effect that degenerate into worst things
 */
case class SaveEffectSpecialRuling(eid: EffectID, text: String) extends Ruling with RulingDecisionHandler[List[TransactionalAction]] {
  def decisionValidator(decision: Decision[_]): Boolean = decision match {
    case SaveEffectSpecialDecision(q, _) => true
    case _ => false
  }

  def processDecision(decision: Decision[_ <: Ruling]): List[TransactionalAction] = {
    decision match {
      case SaveEffectSpecialDecision(q, None) => List(CancelEffect(q.eid))
      case SaveEffectSpecialDecision(q, Some(newEffect)) => List(UpdateEffectCondition(q.eid, Effect.Condition.Generic(newEffect, false)))
      case _ => Nil
    }
  }

}

/**
 * For effect normal effects that save ends
 */
case class SaveEffectRuling(eid: EffectID, text: String) extends Ruling with RulingDecisionHandler[List[TransactionalAction]] {
  def decisionValidator(decision: Decision[_]): Boolean = decision match {
    case SaveEffectDecision(q, _) => true
    case _ => false
  }

  def processDecision(decision: Decision[_ <: Ruling]): List[TransactionalAction] = {
    decision match {
      case SaveEffectDecision(q, true) => List(CancelEffect(q.eid))
      case _ => Nil
    }
  }
}

/**
 * Decision for a normal SaveEffectDecision.
 * @param ruling Referred ruling
 * @param hasSaved True indicate that the save has been done.
 */
case class SaveEffectDecision(ruling: SaveEffectRuling, hasSaved: Boolean) extends Decision[SaveEffectRuling]

/**
 * Decision for a SaveEffectSpecialRuling.
 * @param ruling Referred ruling
 * @param newEffect An option of the new effect None means that save was done, Some(t) means that the new effect should
 * be t
 */
case class SaveEffectSpecialDecision(ruling: SaveEffectSpecialRuling, newEffect: Option[String]) extends Decision[SaveEffectSpecialRuling]

//REGENERATE

case class RegenerateByRuling(eid: EffectID, what: String) extends Ruling {
  protected def decisionValidator(answer: Decision[_]): Boolean = answer match {
    case a: RegenerateByDecision => true
    case _ => false
  }
}

case class RegenerateByDecision(ruling: RegenerateByRuling, amount: Int) extends Decision[RegenerateByRuling]

//ONGOING

case class OngoingDamageRuling(eid: EffectID, what: String) extends Ruling {
  protected def decisionValidator(answer: Decision[_]): Boolean = answer match {
    case a: OngoingDamageDecision => true
    case _ => false
  }
}

case class OngoingDamageDecision(ruling: OngoingDamageRuling, amount: Int) extends Decision[OngoingDamageRuling]

// SUSTAIN EFFECT

case class SustainEffectRuling(eid: EffectID, what: String) extends Ruling {
  protected def decisionValidator(answer: Decision[_]): Boolean = answer match {
    case o: SustainEffectDecision => true
    case _ => false
  }
}

case class SustainEffectDecision(ruling: SustainEffectRuling, hasSaved: Boolean) extends Decision[SustainEffectRuling]
