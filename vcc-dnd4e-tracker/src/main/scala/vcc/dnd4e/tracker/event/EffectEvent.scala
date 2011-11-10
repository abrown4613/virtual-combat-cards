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
package vcc.dnd4e.tracker.event

import vcc.dnd4e.tracker.common._

//Place holder
private object EffectEvent

/**
 * Add effect to a combatant
 */
case class AddEffectEvent(target: CombatantID, source: CombatantID, condition: Condition, duration: Duration) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = iState.lensFactory.combatantEffectList(target).modIfChanged(iState, _.addEffect(source, condition, duration))
}

/**
 * Change a list with an effect transformation.
 * @param target Combatant that owns the EffectList
 * @param transformation EffectList transformation.
 */
case class ChangeEffectListEvent(target: CombatantID, transformation: EffectTransformation) extends CombatStateEvent {
  def transition(iState: CombatState): CombatState = {
    iState.lensFactory.combatantEffectList(target).modIfChanged(iState, s => s.transformAndFilter(transformation))
  }
}


