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
package vcc.dnd4e.tracker.common

import vcc.infra.util.UniquelyIdentified

/**
 * CombatantID defines a unique combatant in the Roster.
 */
trait CombatantID {
  val id: String
  /**
   * Helper method to simplify interaction with the IDGenerator
   * TODO Remove this method
   */
  @deprecated("To be removed")
  def toSymbol(): Symbol = Symbol(id)

  override def toString(): String = "CombatantID(" + id + ")"

  def asNumber: Option[Int]
}

object CombatantID extends UniquenessCache[String, CombatantID] {

  private class StringCombatantID(val id: String) extends CombatantID {
    def asNumber: Option[Int] = None
  }

  private class IntCombatantID(num: Int) extends CombatantID {
    val id: String = num.toString

    val asNumber = Some(num)
  }

  private val numberRE = """([0-9]+)""".r

  protected def valueFromKey(id: String): CombatantID = {
    id.trim match {
      case numberRE(i) => new IntCombatantID(i.toInt)
      case s => new StringCombatantID(s)
    }
  }

  protected def keyFromValue(sym: CombatantID): Option[String] = Some(sym.id)

}


/**
 * Unique identifier for an entry in the InitiativeOrder.
 * @param combId The combatant that own the InitiativeOrder entry
 * @param seq Sequential number to separate different InitiativeOrderID from the same CombatantID.
 */
case class InitiativeOrderID(val combId: CombatantID, val seq: Int) {
  override def toString(): String = "InitiativeOrderID(" + combId.id + ":" + seq + ")"

  def toLabelString(): String = combId.id + (if (seq < 4) superscript(seq) else ":" + seq)

  final private val superscript = "º¹²³"
}

/**
 * Represents the base bonus and a list of rolls that are that Combatant's initiative result.
 * This is used to set initiatives from the User interface.
 */
case class InitiativeDefinition(combId: CombatantID, bonus: Int, inits: List[Int]) {
  def toResult(): List[InitiativeResult] = {
    inits.zipWithIndex.map(p => InitiativeResult(InitiativeOrderID(combId, p._2), bonus, p._1, 0))
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
  def setTieBreak(tb: Int): InitiativeResult = this.copy(tieBreaker = tb)
}

/**
 * Copied from scala.Symbol
 *  This is private so it won't appear in the library API, but
 * abstracted to offer some hope of reusability.  */
private[tracker] abstract class UniquenessCache[K, V >: Null] {

  import java.lang.ref.WeakReference
  import java.util.WeakHashMap
  import java.util.concurrent.locks.ReentrantReadWriteLock

  private val rwl = new ReentrantReadWriteLock()
  private val rlock = rwl.readLock
  private val wlock = rwl.writeLock
  private val map = new WeakHashMap[K, WeakReference[V]]

  protected def valueFromKey(k: K): V

  protected def keyFromValue(v: V): Option[K]

  def apply(name: K): V = {
    def cached(): V = {
      rlock.lock
      try {
        val reference = map get name
        if (reference == null) null
        else reference.get // will be null if we were gc-ed
      }
      finally rlock.unlock
    }
    def updateCache(): V = {
      wlock.lock
      try {
        val res = cached()
        if (res != null) res
        else {
          val sym = valueFromKey(name)
          map.put(name, new WeakReference(sym))
          sym
        }
      }
      finally wlock.unlock
    }

    val res = cached()
    if (res == null) updateCache()
    else res
  }

  def unapply(other: V): Option[K] = keyFromValue(other)
}
