/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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

object Effect {

  /**
   * Determines the duration of an effect.
   */
  abstract class Duration {
    def shortDesc: String

    override def toString() = "Effect.Duration(" + shortDesc + ")"
  }

  object Duration {
    object SaveEnd extends Duration {
      val shortDesc = "SE"
    }

    object SaveEndSpecial extends Duration {
      val shortDesc = "SE*"
    }

    object Stance extends Duration {
      val shortDesc = "Stance"
    }

    object EndOfEncounter extends Duration {
      val shortDesc = "EoE"
    }

    object Other extends Duration {
      val shortDesc = "Other"
    }

    object Limit extends Enumeration {
      val EndOfNextTurn = Value("EoNT")
      val EndOfTurn = Value("EoT")
      val StartOfNextTurn = Value("SoNT")
    }

    /**
     * Round bound duration are durations that change on the limits (startt and end) of
     * rounds.
     * @param id Combatant ID
     * @param limit Round limit (e.g StartOfNextRound, EndOfRound,EndOfNextRound)
     * @param sustain Indicates effect is sustainable
     */
    case class RoundBound(id: InitiativeOrderID, limit: Limit.Value, sustain: Boolean) extends Duration {
      def shortDesc = limit.toString + (if (sustain) "*" else "") + ":" + id.combId.id
    }
  }
}

object Condition {
  case class Mark(marker: CombatantID, permanent: Boolean) extends Condition {
    def description = "Marked by " + marker.id + (if (permanent) " no mark can supersede" else "")
  }

  case class Generic(description: String) extends Condition
}

/**
 * This is the father of all conditions
 */
abstract class Condition {
  def description: String
}

/**
 * Effect representa a condition being applied to a target for a set duration. The proper
 * model should be a set of Conditions, but for the time being one will do. Effect have the
 * following:
 * @param source The symbol of the source of the effect, used to determine alliedness for delay
 * @param condition The condition being applied
 * @param sustainable Indicates power can be sustained
 * @param benefic Indicates if power is good for the target (important for delay)
 * @param duaration An Effect.Duration
 */
case class Effect(source: CombatantID, condition: Condition, benefic: Boolean, duration: Effect.Duration) {
  import Effect._

  def sustainable = duration match {
    case Effect.Duration.RoundBound(c, l, true) => true
    case _ => false
  }

  /**
   * Change duration according to the start of a round of some combatant
   */
  def startRound(cid: InitiativeOrderID): Effect = {
    duration match {
      case Duration.RoundBound(`cid`, Duration.Limit.StartOfNextTurn, sust) => null
      case Duration.RoundBound(`cid`, Duration.Limit.EndOfNextTurn, sust) =>
        Effect(source, condition, benefic, Duration.RoundBound(cid, Duration.Limit.EndOfTurn, sust))
      case _ => this
    }
  }

  /**
   * Change duration according to the start of a round of some combatant
   */
  def endRound(cid: InitiativeOrderID): Effect = {
    duration match {
      case Duration.RoundBound(`cid`, Duration.Limit.EndOfTurn, sust) => null
      case _ => this
    }
  }

  /**
   * Expire effects that end at the rest after combat, this include Stance, EndOfEncounter
   */
  def applyRest(): Effect = {
    duration match {
      case Duration.Stance => null
      case Duration.EndOfEncounter => null
      case _ => this
    }
  }

  /**
   * If the effect is sustainable, bound duration due to sustain
   */
  def sustain(): Effect = {
    duration match {
      case Duration.RoundBound(src, Duration.Limit.EndOfTurn, true) =>
        Effect(source, condition, benefic, Duration.RoundBound(src, Duration.Limit.EndOfNextTurn, true))
      case _ => this
    }
  }

  /**
   * Process delay on change of round
   * @param ally Is the delay Combatant and ally of the owner of this effect
   * @param who Who is delaying
   * @return Effect a changed effect (null if it expired)
   */
  def processDelay(ally: Boolean, who: InitiativeOrderID): Effect = {
    duration match {
      case Duration.RoundBound(`who`, Duration.Limit.EndOfTurn, true) => null
      case Duration.RoundBound(`who`, Duration.Limit.EndOfTurn, false) if (benefic == ally) => null
      case _ => this
    }
  }

  /**
   * Create a new effect with a new condition
   * @param newcond the condition to be updated
   */
  def updateCondition(newcond: Condition): Effect = {
    condition match {
      case dontcate: Condition.Generic => Effect(source, newcond, benefic, duration)
      case _ => this
    }
  }
}