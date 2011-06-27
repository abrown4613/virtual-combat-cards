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
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.common.EffectTransformation._

/**
 * Locates target in initiative order, adds new effect to list.
 * @param target Which combatant to add the effect to
 * @param source Source of the effect
 * @param condition Condition of the effect
 * @param duration Duration of effect
 */
case class AddEffectTransition(target: CombatantID, source: CombatantID, condition: Condition, duration: Duration) extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState = {
    val ell = lf.combatantEffectList(target)
    /*
        val rl = lf.rosterLens
        //TODO: This may not be really needed
        if (!rl.get(iState).isDefinedAt(source))
          throw new IllegalActionException(source + " is not in combat")
    */
    ell.set(iState, ell.get(iState).addEffect(source, condition, duration))
  }
}

/**
 *
 */
case class SustainEffectTransition(eid: EffectID) extends CombatTransition {
  def transition(lf: StateLensFactory, initialState: CombatState): CombatState =
    lf.combatantEffectList(eid.combId).modIfChanged(initialState, s => s.transformAndFilter(sustainEffect(eid)))
}

case class UpdateEffectConditionTransition(eid: EffectID, newCondition: Condition) extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState =
    lf.combatantEffectList(eid.combId).modIfChanged(iState, el => el.transformAndFilter(updateCondition(eid, newCondition)))
}

case class CancelEffectTransition(eid: EffectID) extends CombatTransition {
  def transition(lf: StateLensFactory, iState: CombatState): CombatState =
    lf.combatantEffectList(eid.combId).modIfChanged(iState, el => el.transformAndFilter(cancelEffect(eid)))
}