/**
 *  Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.snapshot

import vcc.dnd4e.tracker.common._
import vcc.dnd4e.domain.tracker.common.CombatantComment

/**
 * Changes on the state to the combatant and combat state
 */
object StateChange {

  /**
   * Changes to the general combat state
   */
  object combat extends Enumeration {
    val MetaData = Value("MetaData")
    val Order = Value("Initaitive Order")
    val Roster = Value("Roster")
    val OrderFirst = Value("Order Head")
  }

  /**
   * Change to the combatant state
   */
  object combatant extends Enumeration {
    val Comment = Value("Comment")
    val Health = Value("Health")
    val Effects = Value("Effects")
    val Definition = Value("Definition")
    val Initiative = Value("Initiative")
  }

  def changeFromAspect(aspect: CombatantAspect): combatant.Value =
    aspect match {
      case s: CombatantComment => StateChange.combatant.Comment
      case h: HealthTracker => StateChange.combatant.Health
      case e: EffectList => StateChange.combatant.Effects
      case d: CombatantRosterDefinition => StateChange.combatant.Definition
      case i: InitiativeTracker => StateChange.combatant.Initiative
    }

  /**
   * Checks if a set of combat Changes include any change to any of: Roster, Order, or OrderFirst change.
   * @param changes Changes to be checked
   */
  def hasAnySequenceChange(changes: Set[combat.Value]) = !((changes & Set(combat.Order, combat.Roster, combat.OrderFirst)).isEmpty)

  /**
   * Checks if a set of combat Changes include any change to any of: Roster or Order.
   * @param changes Changes to be checked
   */
  def hasSequenceChange(changes: Set[combat.Value]) = (changes.contains(combat.Order) || changes.contains(combat.Roster))

}

class StateChange {
  private var _combatant = Map.empty[CombatantID, Set[StateChange.combatant.Value]]
  private var _combat = Set.empty[StateChange.combat.Value]

  private[snapshot] def add(part: StateChange.combat.Value): StateChange = {
    _combat = _combat + part
    this
  }

  private[snapshot] def add(id: CombatantID, part: StateChange.combatant.Value): StateChange = {
    if (_combatant.isDefinedAt(id)) {
      _combatant = _combatant + (id -> (_combatant(id) + part))
    } else {
      _combatant = _combatant + (id -> Set(part))
    }
    this
  }

  def combatantsThatChanged(part: StateChange.combatant.Value): Set[CombatantID] = {
    Set(_combatant.filter(x => x._2.contains(part)).keys.toList: _*)
  }

  def changesTo(id: CombatantID): Set[StateChange.combatant.Value] = if (_combatant.isDefinedAt(id)) _combatant(id) else Set()

  def changes(): Set[StateChange.combat.Value] = _combat

  override def toString: String = "CombatStateChanges(" + _combat + "," + _combatant + ")"

}