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
 * Health base definition for the combatant
 * @param totalHP The total Hit Points
 */
abstract class HealthDefinition(val totalHP: Int) {
  val lowerBound: Int
  val hasTemporaryHP: Boolean
  val combatantType: CombatantType.Value

  private[common] def status(tracker: HealthTracker): HealthStatus.Value = {
    if (tracker.deathStrikes == 3) HealthStatus.Dead
    else if (tracker.currentHP <= 0) HealthStatus.Dying
    else if (tracker.currentHP <= totalHP / 2) HealthStatus.Bloody
    else HealthStatus.Ok
  }
}

/**
 * Character health Definition, with means that this char will die at -bloodied, or has three strikes
 * @param totalHP The total Hit points
 */
case class CharacterHealthDefinition(override val totalHP: Int) extends HealthDefinition(totalHP) {
  val lowerBound = -totalHP / 2
  val hasTemporaryHP = true
  val combatantType = CombatantType.Character
}

/**
 * Monster health Definition, with means that this dies at 0 no strikes
 * @param totalHP The total Hit points
 */
case class MonsterHealthDefinition(override val totalHP: Int) extends HealthDefinition(totalHP) {
  val lowerBound = 0
  val hasTemporaryHP = true
  val combatantType = CombatantType.Monster
}

case object MinionHealthDefinition extends HealthDefinition(1) {
  val lowerBound = 0
  val combatantType = CombatantType.Minion
  val hasTemporaryHP = false
}

/**
 * Health status type.
 */
object HealthStatus extends Enumeration {
  val Ok = Value("Ok")
  val Bloody = Value("Bloody")
  val Dead = Value("Dead")
  val Dying = Value("Dying")
}

/**
 * HealthTracker master object
 */
object HealthTracker {

  /**
   * Create a HealthTracker based on a HealthDefinition
   * @param healthDefinition Base health definition
   */
  def createTracker(healthDefinition: HealthDefinition): HealthTracker = {
    HealthTracker(healthDefinition.totalHP, 0, 0, healthDefinition)
  }

  /**
   * Create tracker from type and hit points.
   */
  private[common] def createTracker(combatantType: CombatantType.Value, hp: Int): HealthTracker = {
    combatantType match {
      case CombatantType.Character => createTracker(CharacterHealthDefinition(hp))
      case CombatantType.Monster => createTracker(MonsterHealthDefinition(hp))
      case CombatantType.Minion => createTracker(MinionHealthDefinition)
    }
  }

}

/**
 * Difference from base healthDefinition
 * @param damage Current hp
 * @param temporaryHP Temp HP
 * @param deathStrikes Current failed death saves
 */
case class HealthTrackerDelta(damage: Int, temporaryHP: Int, deathStrikes: Int)

/**
 * Since character are the most complex, this class implements their logic,
 * Other creatures will have different sub-logic
 * @param currentHP Current hp
 * @param temporaryHP Temp HP
 * @param deathStrikes Current failed death saves
 */
case class HealthTracker(currentHP: Int, temporaryHP: Int, deathStrikes: Int, base: HealthDefinition) extends CombatantAspect {

  def formattedStatus: String = {
    if (status == HealthStatus.Dying)
      base.status(this).toString + " (%d/3)".format(deathStrikes)
    else
      base.status(this).toString + "!!!".substring(0, deathStrikes)
  }

  def formattedHitPoints: String = {
    if (temporaryHP > 0)
      "%d / %d +%d".format(currentHP, base.totalHP, temporaryHP)
    else
      "%d / %d".format(currentHP, base.totalHP)
  }

  private def boundedChange(amount: Int): Int = {
    val n = currentHP + amount
    if (n < base.lowerBound) base.lowerBound
    else if (n > base.totalHP) base.totalHP
    else n
  }

  def applyDamage(amount: Int) = {
    if (amount > temporaryHP) {
      val nchp = boundedChange(-amount + temporaryHP)
      HealthTracker(nchp, 0, if (nchp == base.lowerBound) 3 else deathStrikes, base)
    } else {
      HealthTracker(currentHP, temporaryHP - amount, deathStrikes, base)
    }
  }

  /**
   * Healing dead is not allowed. They most be removed from death before updating hp
   */
  def heal(amount: Int) = {
    if (status != HealthStatus.Dead)
      HealthTracker(boundedChange(if (currentHP < 0) amount - currentHP else amount), temporaryHP, deathStrikes, base)
    else
      this
  }

  /**
   * Set temporary hit points of creature if it can have them, and the new amount is greater the current score.
   * @param amount Amount of temporary hit points
   * @return a new tracker with the modified temporary hit points.
   */
  def setTemporaryHitPoints(amount: Int): HealthTracker = {
    if (base.hasTemporaryHP && amount > temporaryHP) {
      HealthTracker(currentHP, amount, deathStrikes, base)
    } else this
  }

  def failDeathSave(): HealthTracker = {
    if (this.status == HealthStatus.Dying)
      HealthTracker(currentHP, temporaryHP, deathStrikes + 1, base)
    else this
  }

  def status = base.status(this)

  def rest(extended: Boolean): HealthTracker = {
    if (status != HealthStatus.Dead) {
      if (extended) HealthTracker(base.totalHP, 0, 0, base)
      else HealthTracker(currentHP, 0, 0, base)
    } else
      this
  }

  def raiseFromDead(): HealthTracker = {
    if (status == HealthStatus.Dead) {
      HealthTracker(0, temporaryHP, 0, base)
    } else this
  }

  def getDelta: HealthTrackerDelta = {
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