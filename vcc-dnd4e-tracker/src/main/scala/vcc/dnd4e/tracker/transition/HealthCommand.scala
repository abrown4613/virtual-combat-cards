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

import vcc.dnd4e.tracker.common.{CombatState, CombatantID}
import vcc.dnd4e.tracker.event._
import vcc.controller.IllegalActionException
import vcc.tracker.Event

/**
 * Base health modification transition.
 */
abstract class HealthCommand(target: CombatantID) extends CombatStateCommand {
  /**
   * Override this one
   */

  protected def makeTransitionEvent(): CombatStateEvent

  def generateEvents(state: CombatState): List[Event[CombatState]] = {
    if (!state.roster.isDefinedAt(target))
      throw new IllegalActionException("Combatant " + target + " not in combat")

    makeTransitionEvent() :: Nil
  }
}

/**
 * Applies damage to a combatant and ends combat if all combatants in the initiative order are dead.
 * @param target Target of damage
 * @param amount How many hit points to take away.
 */
case class DamageCommand(target: CombatantID, amount: Int) extends CombatStateCommand {

  def generateEvents(iState: CombatState): List[Event[CombatState]] = {
    if (!iState.roster.isDefinedAt(target))
      throw new IllegalActionException("Combatant " + target + " not in combat")

    // We need to check if all combatant are after we apply damage
    val damageEvent = ApplyDamageEvent(target, amount)
    val nState = damageEvent.transition(iState)

    if (nState.rules.areAllCombatantInOrderDead(nState)) {
      damageEvent :: EndCombatEvent :: Nil
    } else {
      damageEvent :: Nil
    }
  }
}

/**
 * Heal a combatant .
 * @param target Target of healing
 * @param amount How many hit points to restore.
 */
case class HealCommand(target: CombatantID, amount: Int) extends HealthCommand(target) {
  protected def makeTransitionEvent(): CombatStateEvent = ApplyHealingEvent(target, amount)
}

/**
 * Set combatant temporary hit points.
 * @param target Target to get temporary hit points.
 * @param amount How many points to set.
 */
case class SetTemporaryHPCommand(target: CombatantID, amount: Int) extends HealthCommand(target) {
  protected def makeTransitionEvent(): CombatStateEvent = SetTemporaryHitPointsEvent(target, amount)
}

/**
 * Tick on of the character failed death saving throws.
 * @param target Target that failed check
 */
case class FailDeathSaveCommand(target: CombatantID) extends HealthCommand(target) {
  protected def makeTransitionEvent(): CombatStateEvent = FailDeathSaveEvent(target)
}

/**
 * Bring target back from dead (to 0 HP and dying).
 * @param target Target to restore
 */
case class RevertDeathCommand(target: CombatantID) extends HealthCommand(target) {
  protected def makeTransitionEvent(): CombatStateEvent = RevertDeathEvent(target)
}
