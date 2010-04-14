/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.model

import common._
import vcc.controller.transaction._

//TODO This class must be taken out and replaced by the vcc.domain.tracker.common  version
@deprecated
trait CombatantAspect

case class CombatantComment(text: String) extends CombatantAspect

//Change notifications in a closed class
//TODO This class must be taken out and replaced by the vcc.domain.tracker.common  version
@deprecated
abstract sealed class CombatStateChange extends ChangeNotification

case class CombatantUpdate(comb: Symbol, obj: CombatantAspect) extends CombatStateChange

case class RosterUpdate(obj: Map[Symbol, CombatantState]) extends CombatStateChange

case class SequenceChange(seq: Seq[Symbol]) extends CombatStateChange

case class CombatStateChanged(change: Seq[CombatStateChange])


class TrackerCombatant(val id: Symbol, val alias: String, val name: String, val healthDef: HealthDefinition, val init: Int, val ctype: CombatantType.Value, val entity: CombatantEntity) {
  private val _health = new Undoable[HealthTracker](HealthTracker.createTracker(healthDef), (uv) => CombatantUpdate(id, uv.value))

  def health = _health.value

  def health_=(nh: HealthTracker)(implicit trans: Transaction) {
    if (nh != _health.value) _health.value = nh
    this
  }

  private val _info = new Undoable[String]("", uv => {CombatantUpdate(id, CombatantComment(uv.value))})

  def info = _info.value

  def info_=(str: String)(implicit trans: Transaction) {_info.value = str; this}

  val it = new Undoable[InitiativeTracker](InitiativeTracker(0, InitiativeState.Reserve), (uv) => {CombatantUpdate(id, uv.value)})

  private val _effects = new Undoable[EffectList](EffectList(Nil), uv => {CombatantUpdate(id, uv.value)})

  /**
   * Return the lists of active effect on the list.
   */
  def effects: EffectList = _effects.value

  def effects_=(nel: EffectList)(implicit trans: Transaction) {
    if (effects != nel)
      _effects.value = nel;
    this
  }

  def toState(): CombatantState = CombatantState(id, alias, entity, _health.value, it.value, info, effects.effects)
}
