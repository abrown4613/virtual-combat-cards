/**
 * Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.transactional

import vcc.controller.transaction.{Transaction, Undoable}
import vcc.dnd4e.model.common.CombatantType
import vcc.dnd4e.domain.tracker.common._
//TODO These classes should be moved to the tracker.common package
import vcc.dnd4e.model.{CombatantEntity}

class Combatant(val definition: CombatantRosterDefinition) {

  //TODO Remove this
  @deprecated
  def projectHealthDef(oldHD: vcc.dnd4e.model.common.HealthDefinition): HealthDefinition =
    oldHD.ctype match {
      case CombatantType.Character => CharacterHealthDefinition(oldHD.totalHP, oldHD.surgeValue, oldHD.healingSurges)
      case CombatantType.Monster => MonsterHealthDefinition(oldHD.totalHP, oldHD.surgeValue, oldHD.healingSurges)
      case CombatantType.Minion => MinionHealthDefinition()
    }

  private val _health = new Undoable[HealthTracker](HealthTracker.createTracker(projectHealthDef(definition.entity.healthDef)), (uv) => CombatantChange(definition.cid, uv.value))
  private val _comment = new Undoable[String]("", uv => {CombatantChange(definition.cid, CombatantComment(uv.value))})
  private val _effects = new Undoable[EffectList](EffectList(Nil), uv => {CombatantChange(definition.cid, uv.value)})

  def health = _health.value

  def health_=(nh: HealthTracker)(implicit trans: Transaction) {
    if (nh != _health.value) _health.value = nh
    this
  }


  def comment = _comment.value

  def comment_=(str: String)(implicit trans: Transaction) {
    if (str != _comment.value) _comment.value = str
    this
  }

  /**
   * Return the lists of active effect on the list.
   */
  def effects: EffectList = _effects.value

  def effects_=(nel: EffectList)(implicit trans: Transaction) {
    if (effects != nel)
      _effects.value = nel;
    this
  }

  def aspectSet() = Set[CombatantAspect](definition, CombatantComment(_comment.value), _effects.value, _health.value)
}