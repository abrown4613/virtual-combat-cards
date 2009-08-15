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
package vcc.model.datastore

class EntityStoreException(msg:String) extends Exception(msg)

/**
 * Implementation of this trait should provide means of loading
 * and storing entities.
 */
trait EntityStore {
  
  /**
   * Store the entity to the store. Entity must be a well formed 
   * entity, since no validation will be done.
   */
  def store(entity: Entity)
  
  /**
   * Load entity from store, the type of the desired entity must be 
   * specified.
   */
  def load(eid:EntityID):Entity
  
  /**
   * Gets all the fields in the summary and places them in a map. 
   * This is needed to executed the callback on loadEntitySummary
   */
  protected def getEntitySummaryMap(eid:EntityID):Map[DatumKey,String]

  /**
   * Get the EntitySummary for a give entity
   * @param eid The Entity ID to get the summary from
   * @return The entity summary if the entity exists
   */
  def loadEntitySummary(eid:EntityID):EntitySummary = {
    val classOption = getEntityClassID(eid)
    if(classOption.isDefined) {
      val meta = EntityFactory.getEntityBuilder(classOption.get)
      meta.createSummaryFromMap(eid,classOption.get,getEntitySummaryMap(eid))
    } else null
  }
  
  /**
   * Determine if entity exists and returns it type
   * @param eid The Entity ID to get the class from
   * @return None if the Entity does not exist or Some(class) if it exists
   */
  def getEntityClassID(eid:EntityID):Option[EntityClassID]  

  /**
   * Delete an object
   */
  def delete(eid:EntityID)
  
  /**
   * Enumerate Entities of a give class.
   * @param classId If null will return all the classes
   */
  def enumerate(classId: EntityClassID):scala.collection.Set[EntityID]
  
  /**
   * Return the next integer number in the repository sequence. This
   * number should be unique with the repository and once created 
   * it must not be reused.
   */
  def nextSequential():Int
}

/**
 *  
 */
trait EntityStoreBuilder {

  /**
   * Destroy and eliminate all data in the repository.
   */
  def destroy(esid:EntityStoreID)
  
  /**
   * Create a new instance of the repository. In this step all necessary steps to create
   * the repository. It should also open the repository.
   */
  def create(esid:EntityStoreID):EntityStore
  
  /**
   * Open an existing respository. If the repository has not been create an exceptions
   * will be thrown.
   */
  def open(esid:EntityStoreID):EntityStore
  
  /**
   * Test if the repository is already created.
   * @return False if the repository does not exist
   */
  def exists(esid:EntityStoreID):Boolean
  
  /**
   * Test if the EntityStoreID is a valid ID for this type of entity store.
   * @return False if the repository does not exist
   */
  def isValidEntityStoreID(esid:EntityStoreID):Boolean
  
}

/**
 * This object will create new instances of EnitityStore based on the subScheme 
 * of the URI.
 */
object EntityStoreFactory {

  private val logger = org.slf4j.LoggerFactory.getLogger("infra")
  
  private var subSchemeMap:Map[String,EntityStoreBuilder] = Map(
    "directory" -> DirectoryEntityStore
  )
  
  /**
   * Register or redefined a new builder for a specific subscheme
   * @param subScheme The subScheme of the EntityStoreID to register
   * @param builder A function that given a EntityStoreID of a subScheme will return the
   * appropriate EntityStore.
   */
  def registerEntityStoreType(subScheme:String, builder:EntityStoreBuilder) {
    subSchemeMap = subSchemeMap + (subScheme -> builder)
  }
  
  /**
   * Create a new entity Store based on the URI
   * @param esid The EntityStoreID
   * @return A new EntityStore or null if the scheme is bad or is not mapped to a builder
   */
  def createStore(esid:EntityStoreID):EntityStore = {
    if(subSchemeMap.isDefinedAt(esid.subScheme)) {
      subSchemeMap(esid.subScheme).create(esid)
    } else {
      logger.debug("Failed to find builder for {}",esid)
      null
    }
  }
  
  /**
   * Open an existant EntityStore based on the URI
   * @param esid The EntityStoreID
   * @return A new EntityStore or null if the scheme is bad or is not mapped to a builder
   */
  def openStore(esid:EntityStoreID):EntityStore = {
    if(subSchemeMap.isDefinedAt(esid.subScheme)) {
      val builder = subSchemeMap(esid.subScheme)
      if(!builder.exists(esid)) {
        logger.error("Entity {} does not exist, cannot open it",esid)
        null //FIXME
      }
      else builder.open(esid)
    } else {
      logger.debug("Failed to find builder for {}",esid)
      null
    }
  }
  
  def getEntityStoreBuilder(esid:EntityStoreID):EntityStoreBuilder = if(subSchemeMap.isDefinedAt(esid.subScheme)) subSchemeMap(esid.subScheme) else null
  
  def exists(esid:EntityStoreID) = {
    val builder = getEntityStoreBuilder(esid)
    if(builder != null) builder.exists(esid)
    else false
  }
}