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
//$Id$
package vcc.dnd4e.tracker.transition

import vcc.dnd4e.tracker.StateLensFactory
import vcc.dnd4e.tracker.common.{CombatState, HealthTracker, CombatantID}

abstract class HealthTransition(target: CombatantID) extends CombatTransition {
  /**
   * Override this one
   */
  protected def transitionHealth(ht: HealthTracker): HealthTracker

  def transition(lf: StateLensFactory, iState: CombatState): CombatState = {
    lf.combatantHealth(target).modIfChanged(iState, ht => transitionHealth(ht))
  }
}

case class DamageTransition(target: CombatantID, amount: Int) extends HealthTransition(target) {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.applyDamage(amount)
}

case class HealTransition(target: CombatantID, amount: Int) extends HealthTransition(target) {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.heal(amount)
}

case class SetTemporaryHPTransition(target: CombatantID, amount: Int) extends HealthTransition(target) {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.setTemporaryHitPoints(amount, false)
}

case class FailDeathSaveTransition(target: CombatantID) extends HealthTransition(target) {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.failDeathSave()
}

case class RevertDeathTransition(target: CombatantID) extends HealthTransition(target) {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.raiseFromDead()
}
