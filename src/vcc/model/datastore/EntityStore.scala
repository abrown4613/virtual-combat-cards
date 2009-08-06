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
  
}

/**
 * This object will create new instances of EnitityStore based on the subScheme 
 * of the URI.
 */
object EntityStoreFactory {

  private var subSchemeMap:Map[String,EntityStoreID => EntityStore] = Map(
    "directory" -> { esid => new DirectoryEntityStore(esid) }
  )
  
  /**
   * Register or redefined a new builder for a specific subscheme
   * @param subScheme The subScheme of the EntityStoreID to register
   * @param builder A function that given a EntityStoreID of a subScheme will return the
   * appropriate EntityStore.
   */
  def registerEntityStoreType(subScheme:String, builder:EntityStoreID => EntityStore) {
    subSchemeMap = subSchemeMap + (subScheme -> builder)
  }
  
  /**
   * Create a new entity Store based on the URI
   * @param esid The EntityStoreID
   * @return A new EntityStore or null if the scheme is bad or is not mapped to a builder
   */
  def createStore(esid:EntityStoreID):EntityStore = {
    if(subSchemeMap.isDefinedAt(esid.subScheme)) {
      subSchemeMap(esid.subScheme)(esid)
    } else {
      null
    }
  }
}