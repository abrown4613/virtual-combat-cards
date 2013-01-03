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
package vcc.dnd4e.tracker.common

/**
 * This is a simple visitor to handled transformation on Effect. It will take an effect
 * apply a transformation and return a new Effect or null if it expired. The use of this Visitor pattern is to allow for
 * mock up during testing.
 */
trait EffectTransformation {

  /**
   * Takes a Effect and transform to a new Effect. If it returns null this means
   * that the effect lost its meaning, most likely it expired.
   */
  def transform(effect: Effect): Effect
}

/**
 * This is a wrapper for transformations that affect a single EffectID. This is used to build EffectTransformation that
 * will only affect a single effect.
 * @param effectId Control to which Effect this transformation will be applied.
 * @param transformation The function to be applied to any effect that matches the EffectID
 */
class TargetedTransformation(effectId: EffectID, transformation: Effect => Effect) extends EffectTransformation {
  def transform(effect: Effect): Effect = if (effectId == effect.effectId) transformation(effect) else effect
}


/**
 * All the EffectTransformation are written here.
 */
object EffectTransformation {

  import Effect._

  /**
   * Will expire effects that end when the Encounter ends.
   */
  case object applyRest extends EffectTransformation {
    def transform(effect: Effect): Effect = {
      effect.duration match {
        case Duration.Stance => null
        case Duration.Rage => null
        case Duration.EndOfEncounter => null
        case _ => effect
      }
    }
  }

  /**
   * Will change duration due to the start of a InitiativeOrderID round.
   */
  case class startRound(cid: InitiativeOrderID) extends EffectTransformation {
    def transform(effect: Effect): Effect = {
      effect.duration match {
        case Duration.RoundBound(`cid`, Duration.Limit.StartOfNextTurn) => null
        case Duration.RoundBound(`cid`, Duration.Limit.EndOfNextTurn) =>
          Effect(effect.effectId, effect.source, effect.condition, Duration.RoundBound(cid, Duration.Limit.EndOfTurn))
        case Duration.RoundBound(`cid`, Duration.Limit.EndOfNextTurnSustain) =>
          Effect(effect.effectId, effect.source, effect.condition, Duration.RoundBound(cid, Duration.Limit.EndOfTurnSustain))
        case _ => effect
      }
    }

  }

  /**
   * Expired effect that end when the round is over
   */
  case class endRound(cid: InitiativeOrderID) extends EffectTransformation {
    def transform(effect: Effect): Effect = {
      effect.duration match {
        case Duration.RoundBound(`cid`, Duration.Limit.EndOfTurn) => null
        case Duration.RoundBound(`cid`, Duration.Limit.EndOfTurnSustain) => null
        case _ => effect
      }
    }
  }

  /**
   * Process delay on change of round
   * @param ally Is the delay Combatant and ally of the owner of this effect
   * @param cid Who is delaying
   * @return Effect a changed effect (null if it expired)
   */
  case class processDelay(ally: Boolean, cid: InitiativeOrderID) extends EffectTransformation {
    def transform(effect: Effect): Effect = {
      effect.duration match {
        case Duration.RoundBound(`cid`, Duration.Limit.EndOfTurnSustain) => null
        case Duration.RoundBound(`cid`, Duration.Limit.EndOfTurn) if (effect.condition.beneficial == ally) => null
        case _ => effect
      }
    }
  }


  /**
   * Will changed duration of Effect if it can be sustained
   */
  case class sustainEffect(effectId: EffectID) extends TargetedTransformation(effectId, effect => {
    effect.duration match {
      case Duration.RoundBound(src, Duration.Limit.EndOfTurnSustain) =>
        Effect(effect.effectId, effect.source, effect.condition, Duration.RoundBound(src, Duration.Limit.EndOfNextTurnSustain))
      case _ => effect
    }
  })

  /**
   * Create a new effect with a new condition if the old condition is not a mark and the effect is the correct one.
   * @param newCondition the condition to be updated
   */
  case class updateCondition(effectId: EffectID, newCondition: Condition) extends TargetedTransformation(effectId, effect => {
    effect.condition match {
      case dontCare: Condition.Generic => Effect(effect.effectId, effect.source, newCondition, effect.duration)
      case _ => effect
    }
  })

  /**
   * Will cancel an effect, i.e. return null for that effect.
   */
  case class cancelEffect(effectId: EffectID) extends TargetedTransformation(effectId, effect => null)

}