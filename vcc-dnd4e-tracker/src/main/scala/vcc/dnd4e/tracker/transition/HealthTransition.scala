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
package vcc.dnd4e.tracker.transition

import vcc.dnd4e.tracker.common.{HealthStatus, CombatState, HealthTracker, CombatantID}

/**
 * Base health modification transition.
 */
abstract class HealthTransition(target: CombatantID) extends CombatTransition {
  /**
   * Override this one
   */
  protected def transitionHealth(ht: HealthTracker): HealthTracker

  def transition(iState: CombatState): CombatState = {
    iState.lensFactory.combatantHealth(target).modIfChanged(iState, ht => transitionHealth(ht))
  }
}

/**
 * Applies damage to a combatant and ends combat if all combatants in the initiative order are dead.
 * @param target Target of damage
 * @param amount How many hit points to take away.
 */
case class DamageTransition(target: CombatantID, amount: Int) extends HealthTransition(target) {

  override def transition(iState: CombatState): CombatState = {
    val nState = super.transition(iState)
    val lf = iState.lensFactory
    val hl = lf.combatantHealth(target)
    if (hl.get(nState).status == HealthStatus.Dead && nState.rules.areAllCombatantInOrderDead(nState)) {
      nState.endCombat()
    } else {
      nState
    }
  }

  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.applyDamage(amount)
}

/**
 * Heal a combatant .
 * @param target Target of healing
 * @param amount How many hit points to restore.
 */
case class HealTransition(target: CombatantID, amount: Int) extends HealthTransition(target) {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.heal(amount)
}

/**
 * Set combatant temporary hit points.
 * @param target Target to get temporary hit points.
 * @param amount How many points to set.
 */
case class SetTemporaryHPTransition(target: CombatantID, amount: Int) extends HealthTransition(target) {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.setTemporaryHitPoints(amount, false)
}

/**
 * Tick on of the character failed death saving throws.
 * @param target Target that failed check
 */
case class FailDeathSaveTransition(target: CombatantID) extends HealthTransition(target) {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.failDeathSave()
}

/**
 * Bring target back from dead (to 0 HP and dying).
 * @param target Target to restore
 */
case class RevertDeathTransition(target: CombatantID) extends HealthTransition(target) {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.raiseFromDead()
}
