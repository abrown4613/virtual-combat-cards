/**
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
package vcc.dnd4e.tracker.common

/**
 * Health base definition for the combatant
 * @param totalHP The total Hitpoints
 */
abstract class HealthDefinition(val totalHP: Int) {
  val lowerBound: Int
  val hasTemporaryHP: Boolean
  val ctype: CombatantType.Value

  def status(tracker: HealthTracker): HealthTracker.Status.Value = {
    if (tracker.deathStrikes == 3) HealthTracker.Status.Dead
    else if (tracker.currentHP <= 0) HealthTracker.Status.Dying
    else if (tracker.currentHP <= totalHP / 2) HealthTracker.Status.Bloody
    else HealthTracker.Status.Ok
  }
}

/**
 * Character health Definition, with means that this char will die at -bloodied, or has three strikes
 * @param totalHP The total Hitpoints
 */
case class CharacterHealthDefinition(override val totalHP: Int) extends HealthDefinition(totalHP) {
  val lowerBound = -totalHP / 2
  val hasTemporaryHP = true
  val ctype = CombatantType.Character
}

/**
 * Monster health Definition, with means that this dies at 0 no strikes
 * @param totalHP The total Hitpoints
 */
case class MonsterHealthDefinition(override val totalHP: Int) extends HealthDefinition(totalHP) {
  val lowerBound = 0
  val hasTemporaryHP = true
  val ctype = CombatantType.Monster
}

case object MinionHealthDefinition extends HealthDefinition(1) {
  val lowerBound = 0
  val ctype = CombatantType.Minion
  val hasTemporaryHP = false
}

/**
 * HealthTracker master object
 */
object HealthTracker {

  object Status extends Enumeration {
    val Ok = Value("Ok")
    val Bloody = Value("Bloody")
    val Dead = Value("Dead")
    val Dying = Value("Dying")
  }

  /**
   * Create a HealthTracker based on a HealthDefinition
   * @param hdef Base health definition
   */
  def createTracker(hdef: HealthDefinition): HealthTracker = {
    HealthTracker(hdef.totalHP, 0, 0, hdef)
  }

  /**
   * Create a HealthTracker based on a HealthDefinition
   * @param hdef Base health definition
   */
  private[common] def createTracker(ctype: CombatantType.Value, hp: Int): HealthTracker = {
    ctype match {
      case CombatantType.Character => createTracker(CharacterHealthDefinition(hp))
      case CombatantType.Monster => createTracker(MonsterHealthDefinition(hp))
      case CombatantType.Minion => createTracker(MinionHealthDefinition)
    }
  }

}

/**
 * Difference from base healthDefinition
 * @param currentHP Current hp
 * @param temporaryHP Temp HP
 * @param deathStrikes Current failed death saves
 * @param surges Sure
 */
case class HealthTrackerDelta(damage: Int, temporaryHP: Int, deathStrikes: Int)

/**
 * Since character are the most complex, this class implements their logic,
 * Other creatures will have different sublogics
 * @param currentHP Current hp
 * @param temporaryHP Temp HP
 * @param deathStrikes Current failed death saves
 * @param bas
 */
case class HealthTracker(currentHP: Int, temporaryHP: Int, deathStrikes: Int, base: HealthDefinition) extends CombatantAspect {
  private def boundedChange(amnt: Int): Int = {
    val n = currentHP + amnt;
    if (n < base.lowerBound) base.lowerBound
    else if (n > base.totalHP) base.totalHP
    else n
  }

  def applyDamage(amnt: Int) = {
    if (amnt > temporaryHP) {
      val nchp = boundedChange(-amnt + temporaryHP)
      HealthTracker(nchp, 0, if (nchp == base.lowerBound) 3 else deathStrikes, base)
    } else {
      HealthTracker(currentHP, temporaryHP - amnt, deathStrikes, base)
    }
  }

  /**
   * Healing dead is not allowed. They most be removed from death before updating hp
   */
  def heal(amnt: Int) = {
    if (status != HealthTracker.Status.Dead)
      HealthTracker(boundedChange(if (currentHP < 0) amnt - currentHP else amnt), temporaryHP, deathStrikes, base)
    else
      this
  }

  def setTemporaryHitPoints(amnt: Int, force: Boolean) = {
    if (base.hasTemporaryHP) {
      if (force) HealthTracker(currentHP, amnt, deathStrikes, base)
      else if (amnt > temporaryHP)
        HealthTracker(currentHP, amnt, deathStrikes, base)
      else this
    } else this
  }

  def failDeathSave(): HealthTracker = {
    if (this.status == HealthTracker.Status.Dying)
      HealthTracker(currentHP, temporaryHP, deathStrikes + 1, base)
    else this
  }

  def status() = base.status(this)

  def rest(extended: Boolean): HealthTracker = {
    if (status != HealthTracker.Status.Dead) {
      if (extended) HealthTracker(base.totalHP, 0, 0, base)
      else HealthTracker(currentHP, 0, 0, base)
    } else
      this
  }

  def raiseFromDead(): HealthTracker = {
    if (status == HealthTracker.Status.Dead) {
      HealthTracker(0, temporaryHP, 0, base)
    } else this
  }

  def getDelta(): HealthTrackerDelta = {
    HealthTrackerDelta(this.base.totalHP - this.currentHP, this.temporaryHP, this.deathStrikes)
  }

  def applyDelta(delta: HealthTrackerDelta): HealthTracker = {
    HealthTracker(this.currentHP - delta.damage, delta.temporaryHP, delta.deathStrikes, this.base)
  }

  def replaceHealthDefinition(hd: HealthDefinition): HealthTracker = {
    val delta = this.getDelta
    val nht = HealthTracker.createTracker(hd)
    nht.applyDelta(delta)
  }
}