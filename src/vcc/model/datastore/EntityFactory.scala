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
package vcc.model.datastore

/**
 * Creates Entity based on entity ID. All entities type should be registered on this
 * factory to be dynamically created during entity load process.
 */
object EntityFactory {
  
  private val logger = org.slf4j.LoggerFactory.getLogger("infra")

  
  private var _registry=scala.collection.immutable.Map.empty[EntityClassID,EntityBuilder]
  
  /**
   * Register a class and constructor for a given class. Will replace prior definition
   * @param classId The name of the class to be registers
   * @param constructor A function that returns of Entity of 
   */
  def registerEntityClass(classId:EntityClassID,builder:EntityBuilder) {
    _registry += (classId -> builder)
  }
  
  /**
   * Check if the class is registered on the Factory
   * @parma classId Class name to get
   * @return true if the class is register
   */
  def isClassDefined(classId:EntityClassID):Boolean = _registry.contains(classId)
  
  /**
   * Create a new instance of the class if it's registered
   * @param soruce Source of the entity
   * @return A new loaded Entity, if the class exists, null otherwise
   */
  def createInstance(source:EntitySource):Entity = {
    val entity = if(isClassDefined(source.classId)) _registry(source.classId).createInstance(source.id) else null
    if(entity==null) logger.error("EntityFactory does not contain class {}",source.classId)
    if(entity != null) {
	  for(datum <- source.getData()) {
		entity.loadDatum(datum)
	  }
    }
    entity
  }
  
  def getEntityBuilder(classId: EntityClassID):EntityBuilder = 
    if(isClassDefined(classId)) _registry(classId)
    else null

  /**
   * Create a new instance of the class if it's registered
   * @param classID the class ID that should be created
   * @param id The entity ID
   * @return A new empty Entity, if the class exists, null otherwise
   */
  def createInstance(classId:EntityClassID, id:EntityID):Entity = {
    if(isClassDefined(classId)) _registry(classId).createInstance(id)
    else null
  }

}
