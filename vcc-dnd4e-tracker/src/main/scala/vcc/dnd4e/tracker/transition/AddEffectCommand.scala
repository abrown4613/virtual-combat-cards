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

import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.common.EffectTransformation._
import vcc.controller.IllegalActionException
import vcc.dnd4e.tracker.event.{ChangeEffectListEvent, AddEffectEvent, CombatStateEvent}

/**
 * Locates target in initiative order, adds new effect to list.
 * @param target Which combatant to add the effect to
 * @param source Source of the effect
 * @param condition Condition of the effect
 * @param duration Duration of effect
 */
case class AddEffectCommand(target: CombatantID, source: CombatantID, condition: Condition, duration: Duration) extends CombatStateCommand {
  def generateTransitions(iState: CombatState): List[CombatStateEvent] = {
    if (!iState.roster.isDefinedAt(target))
      throw new IllegalActionException(target + " not in roster")
    AddEffectEvent(target, source, condition, duration) :: Nil
  }
}

/**
 * Update duration to sustain an effect
 */
case class SustainEffectCommand(eid: EffectID) extends CombatStateCommand {
  def generateTransitions(iState: CombatState): List[CombatStateEvent] = {
    if (!iState.roster.isDefinedAt(eid.combId))
      throw new IllegalActionException(eid.combId + " not in roster")
    ChangeEffectListEvent(eid.combId, sustainEffect(eid)) :: Nil
  }
}

/**
 * Update condition for an effect
 *
 */
case class UpdateEffectConditionCommand(eid: EffectID, newCondition: Condition) extends CombatStateCommand {
  def generateTransitions(iState: CombatState): List[CombatStateEvent] = {
    if (!iState.roster.isDefinedAt(eid.combId))
      throw new IllegalActionException(eid.combId + " not in roster")
    ChangeEffectListEvent(eid.combId, updateCondition(eid, newCondition)) :: Nil
  }
}

/**
 * Cancel effect
 */
case class CancelEffectCommand(eid: EffectID) extends CombatStateCommand {
  def generateTransitions(iState: CombatState): List[CombatStateEvent] = {
    if (!iState.roster.isDefinedAt(eid.combId))
      throw new IllegalActionException(eid.combId + " not in roster")
    ChangeEffectListEvent(eid.combId, cancelEffect(eid)) :: Nil
  }
}