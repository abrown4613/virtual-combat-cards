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
package vcc.dnd4e.tracker.common

/**
 *
 */
case class Combatant(definition: CombatantRosterDefinition, comment: String, health: HealthTracker, effects: EffectList) {
  def updateDefinition(newDef: CombatantRosterDefinition): Combatant = {
    this.copy(
      definition = newDef,
      health = this.health.replaceHealthDefinition(newDef.entity.healthDef)
    )
  }
}

/**
 *  Defines the major CombatantRoster data for a tracker.transactional.Combatant, it is also used for views.
 */
case class CombatantRosterDefinition(cid: CombatantID, alias: String, entity: CombatantEntity) extends CombatantAspect

object Combatant {
  def apply(definition: CombatantRosterDefinition): Combatant = {
    Combatant(
      definition = definition,
      health = HealthTracker.createTracker(definition.entity.healthDef),
      effects = EffectList(definition.cid, Nil),
      comment = ""
    )
  }

  case object RosterFactory extends RosterCombatantFactory[Combatant] {

    def replaceDefinition(combatant: Combatant, newDefinition: CombatantRosterDefinition): Combatant = {
      combatant.updateDefinition(newDefinition)
    }

    def createCombatant(definition: CombatantRosterDefinition): Combatant = apply(definition)
  }

}
