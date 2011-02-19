/**
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
package vcc.dnd4e.model

import common.CombatantType
import vcc.dnd4e.domain.tracker.common.{CharacterHealthDefinition, MonsterHealthDefinition, HealthDefinition}
import vcc.infra.datastore.naming.EntityID
import vcc.dnd4e.domain.compendium.{CombatantEntity => CompendiumCombatantEntity, MonsterEntity, CharacterEntity}
import vcc.domain.dndi.CaptureTemplateEngine
import vcc.infra.xtemplate.{MapDataSource}

case class CombatantEntity(eid: EntityID, name: String, healthDef: HealthDefinition, initiative: Int, ctype: CombatantType.Value, statBlock: String) {
  override def toString(): String = "CombatantEntity(" + eid + "," + name + "," + healthDef + "," + initiative + "," + ctype + ")"
}

object CombatantEntity {

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
      val template = CaptureTemplateEngine.fetchClassTemplate(comp.classID.shortClassName)
      val dse = comp.asDataStoreEntity
      template.render(new MapDataSource(dse.data, Map(), Map())).toString
    }
    CombatantEntity(comp.eid, comp.name.value, healthDef, comp.initiative.value, comp.combatantType, statBlock)
  }
}
