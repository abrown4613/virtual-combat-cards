/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
 * An EffectID allows for the identification of the owning Combatant and an unique sequence number in the list of effect
 * of that Combatant.
 * @param combId CombatantID of the owning combatant
 * @param seq Sequential and unique number of the effect in that combatant
 */
case class EffectID(combId: CombatantID, seq: Int)

/**
 *  This is the father of all conditions
 */
abstract class Condition(val beneficial: Boolean) {
  def description: String
}

/**
 *  Determines the duration of an effect.
 */
abstract class Duration(initDesc: String) {
  def shortDescription = initDesc

  def this() = this (null)

  override def toString = "Effect.Duration(" + shortDescription + ")"
}

object Duration {
  object SaveEnd extends Duration("SE")

  object SaveEndSpecial extends Duration("SE*")

  object Stance extends Duration("Stance")

  object Rage extends Duration("Rage")

  object EndOfEncounter extends Duration("EoE")

  object Other extends Duration("Other")

  object Limit extends Enumeration {
    val EndOfNextTurn = Value("EoNT")
    val EndOfTurn = Value("EoT")
    val StartOfNextTurn = Value("SoNT")
    val EndOfNextTurnSustain = Value("EoNT*")
    val EndOfTurnSustain = Value("EoT*")
  }

  /**
   * Round bound duration are durations that change on the limits (startt and end) of
   * rounds.
   * @param id Combatant ID
   * @param limit Round limit (e.g StartOfNextRound, EndOfRound,EndOfNextRound)
   */
  case class RoundBound(id: InitiativeOrderID, limit: Limit.Value) extends Duration {
    override def shortDescription: String = limit.toString + ":" + id.toLabelString
  }

  def staticDurationFromDescription(staticDuration: String):Option[Duration] = {
     allStaticDurations.find(x => x.shortDescription == staticDuration)
  }

  val allStaticDurations = Seq(EndOfEncounter, SaveEnd,SaveEndSpecial, Stance, Rage, Other)
}

/**
 * Effect object provides helper functions and definitions to be used with the Effect case class.
 */
object Effect {

  /**
   * Build an Effect without an EffectID, this should be added by the EffectList
   * @param source CombatantID that caused the effect
   * @param condition The condition  that is caused by the effect
   * @param duration Duration of the effect
   * @return An Effect with no EffectID
   */
  def apply(source: CombatantID, condition: Condition, duration: Duration): Effect =
    Effect(null, source, condition, duration)

  /**
   * Condition of the effect, currently this is either a Mark or a generic text.
   */
  object Condition {

    case class Mark(marker: CombatantID, permanent: Boolean) extends Condition(false) {
      def description = "Marked by " + marker.id + (if (permanent) " no mark can supersede" else "")
    }

    case class Generic(description: String, override val beneficial: Boolean) extends Condition(beneficial)

  }
}

/**
 * Effect represents a condition being applied to a target for a set duration. The proper
 * model should be a set of Conditions, but for the time being one will do. Effect have the
 * following:
 * @param effectId The unique identifier of an effect in the EffectList, may be null for effects that have not been
 * added to a list.
 * @param source The symbol of the source of the effect, used to determine alliedness for delay
 * @param condition The condition being applied
 * @param duration An Effect.Duration
 */
case class Effect(effectId: EffectID, source: CombatantID, condition: Condition, duration: Duration) {

  def sustainable = duration match {
    case Duration.RoundBound(c, l) => (l == Duration.Limit.EndOfNextTurnSustain) || (l == Duration.Limit.EndOfTurnSustain)
    case _ => false
  }
}