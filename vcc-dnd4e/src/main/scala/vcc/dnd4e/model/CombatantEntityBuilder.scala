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
package vcc.dnd4e.model

import vcc.dnd4e.compendium.{CombatantEntity => CompendiumCombatantEntity, MonsterEntity, CharacterEntity}
import vcc.dndi.app.CaptureTemplateEngine
import vcc.infra.xtemplate.{MapDataSource}
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.compendium.{CombatantType => CompendiumCombatantType}

object CombatantEntityBuilder {

  /**
   * Build a valid combatant form a CompendiumEntity
   */
  def fromCompendiumCombatantEntity(comp: CompendiumCombatantEntity): CombatantEntity = {
    val healthDef: HealthDefinition = comp match {
      case monster: MonsterEntity => MonsterHealthDefinition(monster.hp.value)
      case character: CharacterEntity => CharacterHealthDefinition(comp.hp.value)
      case s => throw new Exception("Unexpected Entity type: " + s)
    }
    val statBlock = if (comp.statblock.isDefined) {
      comp.statblock.value
    } else {
      val template = CaptureTemplateEngine.fetchClassTemplate(comp.classID.shortClassName())
      val dse = comp.asDataStoreEntity()
      template.render(new MapDataSource(dse.data, Map(), Map())).toString()
    }
    CombatantEntity(comp.eid.asStorageString, comp.name.value, healthDef, comp.initiative.value, mapCombatantType(comp.combatantType), statBlock)
  }

  private def mapCombatantType(compendiumType: CompendiumCombatantType.Value): CombatantType.Value = {
    CombatantType.withName(compendiumType.toString)
  }
}
