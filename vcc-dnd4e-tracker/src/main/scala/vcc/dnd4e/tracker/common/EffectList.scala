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

/**
 * A list of Effect that affect a combatant.
 * @param target The CombatantID  that owns this list. This is used to generate EffectID
 * @param effects All the effects on the target
 */
case class EffectList(target: CombatantID, effects: List[Effect]) extends CombatantAspect {

  import Effect._

  private[common] def maxEffectID: Int = {
    var max = 0
    for (effect <- effects) if (effect.effectId.seq >= max) max = effect.effectId.seq
    max
  }

  /**
   * Add an Effect to the list. This process will generate a new EffectID (unique with regards to the other) and
   * will do the following:
   * <ul>
   * <li>Replace a mark if a new mark is added
   * <li>Keep permanent marks (even if a new mark comes along)
   * <li>Replace Stance and Rage durations
   * <li>Add new Effect to the head of the list.
   * </ul>
   * @param source CombatantID  that caused the effect
   * @param condition The condition being caused by this effect
   * @param duration The Duration of the effect.
   * @return A new EffectList with the new effect if it can be added.
   */
  def addEffect(source: CombatantID, condition: Condition, duration: Duration): EffectList = {
    val effectId = EffectID(target, maxEffectID + 1)

    //Don't replace a permanent mark
    if (condition.isInstanceOf[Condition.Mark] && effects.exists(e => e.condition match {
      case Condition.Mark(someone, true) => true
      case _ => false
    })) return this

    val filteredEffects: List[Effect] = {
      if (duration == Duration.Stance || duration == Duration.Rage)
        effects.filter(e => e.duration != duration)
      else if (condition.isInstanceOf[Condition.Mark])
        effects.filter(e => !e.condition.isInstanceOf[Condition.Mark])
      else effects
    }

    EffectList(target, Effect(effectId, source, condition, duration) :: filteredEffects)
  }

  /**
   * Applies a EffectTransformation to all the effects in the list and then filters out any effect that was transformed
   * to null.
   * @param transform The transformation to be applied to the effect
   * @return A list of effects with new values for the effects and any effect that was transformed to null removed.
   */
  def transformAndFilter(transformation: EffectTransformation): EffectList = {
    EffectList(target, effects.map(e => transformation.transform(e)).filter(e => e != null))
  }

}
