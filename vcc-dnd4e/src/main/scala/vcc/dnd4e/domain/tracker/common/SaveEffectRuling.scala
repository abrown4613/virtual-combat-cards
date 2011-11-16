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
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.common.Command.{CombatStateAction, CancelEffect}

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
 * For effect normal effects that save ends
 * @param eid Effect identifier
 * @param condition Condition to save from.
 */
case class SaveEffectRuling(eid: EffectID, condition: String) extends Ruling with RulingDecisionHandler[List[CombatStateAction]] {
  def decisionValidator(decision: Decision[_]): Boolean = decision match {
    case SaveEffectDecision(q, _) => true
    case _ => false
  }

  def processDecision(decision: Decision[_ <: Ruling]): List[CombatStateAction] = {
    decision match {
      case SaveEffectDecision(q, SaveEffectDecision.Saved) => List(CancelEffect(q.eid))
      case _ => Nil
    }
  }
}

object SaveEffectDecision extends Enumeration {
  val Saved = Value("Saved")
  val Failed = Value("Failed")
}

/**
 * Decision for a normal SaveEffectDecision.
 * @param ruling Referred ruling
 * @param hasSaved True indicate that the save has been done.
 */
case class SaveEffectDecision(ruling: SaveEffectRuling, result: SaveEffectDecision.Value) extends Decision[SaveEffectRuling]


case class SaveVersusDeathRuling(comb: CombatantID) extends Ruling with RulingDecisionHandler[List[CombatStateAction]] {
  protected def decisionValidator(decision: Decision[_]): Boolean = decision match {
    case e: SaveVersusDeathDecision => true
    case _ => false
  }

  def processDecision(decision: Decision[_ <: Ruling]): List[CombatStateAction] = {
    decision match {
      case SaveVersusDeathDecision(ruling, SaveVersusDeathDecision.Failed) => List(Command.FailDeathSave(ruling.comb))
      case SaveVersusDeathDecision(ruling, SaveVersusDeathDecision.SaveAndHeal) => List(Command.HealDamage(ruling.comb, 1))
      case _ => Nil
    }
  }
}

object SaveVersusDeathDecision extends Enumeration {
  val Saved = Value("Saved")
  val Failed = Value("Failed")
  val SaveAndHeal = Value("Save and heal 1 hp")
}

case class SaveVersusDeathDecision(ruling: SaveVersusDeathRuling, result: SaveVersusDeathDecision.Value) extends Decision[SaveVersusDeathRuling]