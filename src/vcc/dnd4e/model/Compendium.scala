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

import vcc.model.datastore._
import vcc.infra.startup.StartupStep

object Compendium extends StartupStep {
  val monsterClassID = DataStoreURI.asEntityClassID("vcc-class:monster")
  val characterClassID = DataStoreURI.asEntityClassID("vcc-class:character")
  var _activeRepository:EntityStore = null
  val logger = org.slf4j.LoggerFactory.getLogger("domain")

  def setActiveRepository(es: EntityStore) {
    _activeRepository = es
  }
  
  def activeRepository: EntityStore = _activeRepository
  
  EntityFactory.registerEntityClass(monsterClassID, MonsterEntityBuilder)
  EntityFactory.registerEntityClass(characterClassID, CharacterEntityBuilder)
  
  def isStartupComplete = 
    EntityFactory.isClassDefined(monsterClassID) && 
    EntityFactory.isClassDefined(characterClassID)
  
  
  def createNewOfficialMonster(id:Int):MonsterEntity = {
    val eid = DataStoreURI.asEntityID("vcc-ent:dndi:monster:"+id)
    logger.debug("Creating new instance {} of class {}",eid,Compendium.monsterClassID)
    EntityFactory.createInstance(Compendium.monsterClassID,eid).asInstanceOf[MonsterEntity]
  }
  
  private def generateEntityID(prefix:String):EntityID = { 
    if(_activeRepository == null) throw new Exception("Must initialize repository prior to creating instances")
    val id = _activeRepository.nextSequential
    DataStoreURI.asEntityID("vcc-ent:"+prefix+":"+id)
  } 
  
  def createNewMonster():MonsterEntity = {
    val eid = generateEntityID("monster")
    logger.debug("Creating new instance {} of class {}",eid,Compendium.monsterClassID)
    EntityFactory.createInstance(Compendium.monsterClassID,eid).asInstanceOf[MonsterEntity]
  }
  
  def createNewCharacter():CharacterEntity = {
    val eid = generateEntityID("pc")
    logger.debug("Creating new instance {} of class {}",eid,Compendium.characterClassID)
    EntityFactory.createInstance(Compendium.characterClassID,eid).asInstanceOf[CharacterEntity]
    
  }
  
}

