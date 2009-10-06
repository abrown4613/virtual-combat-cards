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
package vcc.dnd4e.domain.compendium

import vcc.infra.datastore.naming._
import vcc.infra.datastore.DataStoreFactory
import vcc.infra.startup.StartupStep

class CompendiumRepository(dsuri:DataStoreURI) extends StartupStep {
  val logger = org.slf4j.LoggerFactory.getLogger("domain")
  
  val dataStore = DataStoreFactory.getDataStoreBuilder(dsuri).open(dsuri)
  private val monsterSummaries = scala.collection.mutable.Map.empty[EntityID,MonsterSummary] 
  private val characterSummaries = scala.collection.mutable.Map.empty[EntityID,CharacterSummary] 
  
  initialize()
  
  private def initialize() {
	if(dataStore == null) throw new Exception("Failed to open datastore "+dsuri)
	val ents = dataStore.extractEntityData(Set("classid","base:level","base:xp","base:role","base:class","base:name","base:race","stat:hp"))
	for(ent<-ents) {
	  storeSummary(ent._1,ent._2)
    }
  }
  
  def isStartupComplete = dataStore != null

  protected def storeSummary(eid:EntityID,fields:Map[String,String]) {
    fields.getOrElse("classid",null) match {
      case "vcc-class:monster" => 
        val ent = MonsterSummary.fromFieldMap(eid,fields)
        if(ent != null) monsterSummaries += (eid ->ent)
      case "vcc-class:character" => 
        val ent = CharacterSummary.fromFieldMap(eid,fields)
        if(ent != null) characterSummaries += (eid ->ent)
      case s =>
        println("Can't handle: "+s)
        println("Fields:"+fields)
        null
    }
  }
  
  def createEntity(classid:EntityClassID,eid:EntityID):CombatantEntity = null

  def store(entity:CombatantEntity):Boolean = false
  
  /**
   * Load an CombatantEntity
   * @param eid EntityID to be fetched
   * @param mustBeValid Indicate whether a valid entity is required (true) or not
   * @return The entity if it is found and respects the mustBeValid option
   */
  def load(eid:EntityID,mustBeValid:Boolean):CombatantEntity = {
    def getEmptyEntity(eid:EntityID, fields:Map[String,String]):CombatantEntity = {
      fields.getOrElse("classid", null) match {
        case "vcc-class:monster" => new MonsterEntity(eid)
        case "vcc-class:character" => new CharacterEntity(eid)
        case s => 
          logger.warn("Entity loaded with unknown class {}",s)
          null
      }
    }
    val dse = dataStore.loadEntity(eid)
    if(dse != null) {
      logger.debug("Loaded Entity {} from datastore, content: {}",eid,dse)
      val ent = getEmptyEntity(dse.eid,dse.data)
      logger.debug("Empty container {}",ent)
      if(ent!=null) {
        ent.loadFromMap(dse.data)
        logger.debug("Loaded entity, is it valid? {}",ent.isValid)
        ent.dump(logger)
      }
      if(mustBeValid)
    	if(ent != null && ent.isValid) ent else null
      else ent
    } else null
  }
  
  def delete(eid:EntityID):Boolean = false
  
  def getMonsterSummaries():Seq[MonsterSummary] = monsterSummaries.values.toList

  def getCharacterSummaries():Seq[CharacterSummary] = characterSummaries.values.toList
  
  def getEntitySummary(eid:EntityID):EntitySummary = {
    if(monsterSummaries.isDefinedAt(eid)) return monsterSummaries(eid)
    if(characterSummaries.isDefinedAt(eid)) return characterSummaries(eid)
    null
  }
  
}
