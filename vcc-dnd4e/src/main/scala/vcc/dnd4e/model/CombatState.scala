//$Id$

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

package vcc.dnd4e.model

import common._

/**
 * State of a combatant
 */
case class CombatantState(id: Symbol, alias: String, entity: CombatantEntity, health: HealthTracker, init: InitiativeTracker, info: String, effects: List[Effect]) {
  def prettyPrint() {
    println("Combatant[" + id + "/" + alias + "]" + entity.name)
    println("  - Health: " + health)
    println("  - Init  : " + init)
    println("  - Info  : " + info)
    println("  - Effect: " + effects)
  }

  def isCharacter: Boolean = health.base.ctype == CombatantType.Character

}

object CombatantState {
  def apply(id: Symbol, alias: String, entity: CombatantEntity): CombatantState = CombatantState(id, alias, entity, null, null, null, null)

  object part extends Enumeration {
    val Health = Value("Health")
    val Initiative = Value("Initiative")
    val Note = Value("Notes")
    val Effects = Value("Effects")
  }
}

/**
 * State of the entire combat.
 */
case class CombatState(combatantMap: Map[Symbol, CombatantState], combatantSequence: Seq[CombatantState]) {
  def prettyPrint() {
    println("------ COMBAT STATE ------")
    for (e <- combatantSequence) {
      e.prettyPrint
    }
    println("---- END COMBAT STATE ----")
  }

  /**
   * This is an utility method to get the a combantant from a ID option.
   * @param id Option of the ID
   * @return Option[CombatantState] A combatant if the ID defines on, or None otherwise.
   */
  def getCombatant(id: Option[Symbol]): Option[CombatantState] = {
    if (id.isDefined && combatantMap.isDefinedAt(id.get))
      Some(combatantMap(id.get))
    else None
  }
}

object CombatState {
  object part extends Enumeration {
    val Sequence = Value("Sequence")
  }
}

/**
 * Place holder
 */
class CombatStateChanges {
  private var _combatant = Map.empty[Symbol, Set[CombatantState.part.Value]]
  private var _combat = Set.empty[CombatState.part.Value]

  private[model] def add(part: CombatState.part.Value): CombatStateChanges = {
    _combat = _combat + part
    this
  }

  private[model] def add(id: Symbol, part: CombatantState.part.Value): CombatStateChanges = {
    if (_combatant.isDefinedAt(id)) {
      _combatant = _combatant + (id -> (_combatant(id) + part))
    } else {
      _combatant = _combatant + (id -> Set(part))
    }
    this
  }

  def combatantsThatChange(part: CombatantState.part.Value): Set[Symbol] = {
    Set(_combatant.filter(x => x._2.contains(part)).keys.toList: _*)
  }

  def changesTo(id: Symbol): Set[CombatantState.part.Value] = if (_combatant.isDefinedAt(id)) _combatant(id) else Set()

  def changes(): Set[CombatState.part.Value] = _combat

  override def toString(): String = "CombatStateChanges(" + _combat + "," + _combatant + ")"
}

/**
 * Any object that observes changes in the combat state should listen to this
 */
trait CombatStateObserver {
  def combatStateChanged(newState: CombatState, changes: CombatStateChanges)
}

