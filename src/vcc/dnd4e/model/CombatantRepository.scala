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

import vcc.infra.datastore.naming.EntityID
import vcc.dnd4e.domain.compendium.{CombatantEntity=>CompendiumCombatantEntity,MonsterEntity,CharacterEntity}

case class CombatantEntityID(ceid:Int)

case class CombatantEntity(eid:EntityID, name:String, healthDef:HealthDefinition, initiative:Int, ctype:CombatantType.Value, statBlock:String)

object CombatantEntity {

  /**
   * Build a valid comabant form a CompendiumEntity
   */
  def fromCompendiumCombatantEntity(comp:CompendiumCombatantEntity):CombatantEntity = {
	val healthDef:HealthDefinition = comp match {
	  case monster:MonsterEntity => MonsterHealthDefinition(monster.hp.value,monster.hp.value/4,(monster.level.value+9)/10)
	  case character:CharacterEntity => CharacterHealthDefinition(comp.hp.value,comp.hp.value/4,15) //TODO (add surege count)
	  case s => throw new Exception("Unexpected Entity type: "+s) 
	}
  	val statBlock = if(comp.statblock.isDefined) comp.statblock.value else vcc.dnd4e.view.helper.CombatantStatBlockCache.generateMiniBlock(comp)
 println("Stat block: "+statBlock)
  	CombatantEntity(comp.eid,comp.name.value,healthDef,comp.initiative.value,comp.combatantType,statBlock)
  }
}

/**
 * This object provide as symbolic link between EntityID and the 
 * CombatantEntity. It is used as an adapter between the compendium objects
 * and the tracks. 
 */
object CombatantRepository {
  
  private var nextID = 1
  
  private val ents = scala.collection.mutable.Map.empty[CombatantEntityID,CombatantEntity]
  
  /**
   * Return the entity defined by the ceid parameter
   * @param ceid CombantantEntityID local to this repository
   * @return A CombatantEntity or null 
   */
  def getEntity(ceid:CombatantEntityID):CombatantEntity = if(ents.isDefinedAt(ceid)) ents(ceid) else null
  
  /**
   * Return the entity defined by the ceid parameter
   * @param ce CombantantEntity to be stored, no critic is done on existing objects
   * @return The new object CombatantEntityID
   */
  def registerEntity(ce:CombatantEntity):CombatantEntityID = {
    synchronized {
      val neid = CombatantEntityID(nextID)
      nextID = nextID + 1
      ents += (neid -> ce)
      neid
    }
  }
  
}
