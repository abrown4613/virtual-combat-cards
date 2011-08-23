package vcc.dnd4e.tracker.event

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

import vcc.dnd4e.tracker.common.{HealthTracker, CombatState, CombatantID}

trait HealthEvent extends CombatStateEvent {
  val target: CombatantID

  protected def transitionHealth(ht: HealthTracker): HealthTracker

  def transition(iState: CombatState): CombatState = iState.lensFactory.combatantHealth(target).modIfChanged(iState, ht => transitionHealth(ht))
}

case class ApplyDamageEvent(target: CombatantID, amount: Int) extends HealthEvent {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.applyDamage(amount)
}

case class ApplyHealingEvent(target: CombatantID, amount: Int) extends HealthEvent {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.heal(amount)
}

case class SetTemporaryHitPointsEvent(target: CombatantID, amount: Int) extends HealthEvent {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.setTemporaryHitPoints(amount, false)
}

case class FailDeathSaveEvent(target: CombatantID) extends HealthEvent {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.failDeathSave()
}

case class RevertDeathEvent(target: CombatantID) extends HealthEvent {
  protected def transitionHealth(ht: HealthTracker): HealthTracker = ht.raiseFromDead()
}