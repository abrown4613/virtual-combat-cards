/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.tracker.common

/**
 * Provides access to immutable information on Combatant
 */
trait CombatantStateView {
  def health: HealthTracker

  def effects: EffectList

  def definition: CombatantRosterDefinition

  def comment: String

  def name: String = definition.entity.name

  def combatantType = definition.entity.combatantType
}

/**
 * Provides access to immutable information about the Combat
 */
trait CombatStateView {
  def combatantViewFromID(id: CombatantID): CombatantStateView

  def allCombatantIDs: List[CombatantID]

  def initiativeTrackerFromID(ioid: InitiativeOrderID): InitiativeTracker

  def getInitiativeOrder: List[InitiativeOrderID]

  def isCombatStarted: Boolean

  def combatComment: String

  def nextUp: Option[InitiativeOrderID]

  /**
   * Returns a sequence of all effect in the combat
   * @return Sequence of all effect in all combatants
   */
  def allEffects(): Seq[Effect] = allCombatantIDs.map(id => combatantViewFromID(id).effects).flatMap(elist => elist.effects)
}