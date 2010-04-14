/**
 * Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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

import vcc.infra.util.UniquelyIdentified

case class CombatantID(id: String)

case class InitiativeOrderID(val combId: CombatantID, val seq: Int) {
  override def toString(): String = "InitiativeOrderID(" + combId.id + ":" + seq + ")"

  def toLabelString(): String = combId.id + ":" + seq
}

/**
 * Represents the base bonus and a list of rolls that are that Combatant's initiative result.
 * This is used to set initiatives from the User interface.
 */
case class InitiativeDefinition(combId: CombatantID, bonus: Int, inits: List[Int]) {
  def toResult(): List[InitiativeResult] = {
    inits.zip((0 to inits.length).toList).map(p => InitiativeResult(InitiativeOrderID(combId, p._2), bonus, p._1, 0))
  }
}

/**
 * Represent the initiative result of a single InitiativeOrderID.
 * @param uniqueId The initiative order identifier for this entry
 * @param bonus Initiative bonus for unties
 * @param result The dice result (including the bonus)
 * @param tieBreaker This is a number to indicate relative order between element with same result and bonus.
 */
case class InitiativeResult(override val uniqueId: InitiativeOrderID, bonus: Int, result: Int, tieBreaker: Int)
        extends Ordered[InitiativeResult] with UniquelyIdentified[InitiativeOrderID] {

  /**
   * Compare to result with the following logic.
   * 1) Biggest result first
   * 2) largest bonus next
   * 3) If both have tie-breaker largest of those.
   */
  def compare(that: InitiativeResult): Int =
    if (this.result == that.result) {
      if (this.bonus == that.bonus) {
        //Check for tie-Breakr
        if (this.tieBreaker != 0 && that.tieBreaker != 0)
          if (this.tieBreaker < that.tieBreaker) -1
          else 1
        else 0 // Need to say we have a tie
      } else
        this.bonus - that.bonus
    }
    else
      this.result - that.result

  /**
   * Return a copy of the object with the tieBreaker set
   * @param tb New value for the tieBreaker
   * @return A new InitiativeResult object
   */
  def setTieBreak(tb: Int): InitiativeResult = InitiativeResult(uniqueId, bonus, result, tb)
}
