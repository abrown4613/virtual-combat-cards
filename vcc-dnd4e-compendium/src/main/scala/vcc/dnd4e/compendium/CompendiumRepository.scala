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
package vcc.dnd4e.compendium

import vcc.infra.datastore.naming._
import vcc.infra.datastore.DataStoreFactory
import vcc.infra.startup.StartupStep
import scala.ref.WeakReference

trait CompendiumRepositoryObserver {
  def compendiumChanged()
}

class CompendiumRepository(dsuri: DataStoreURI) extends StartupStep {
  val logger = org.slf4j.LoggerFactory.getLogger("domain")

  val dataStore = DataStoreFactory.getDataStoreBuilder(dsuri).open(dsuri)
  private val monsterSummaries = scala.collection.mutable.Map.empty[EntityID, MonsterSummary]
  private val characterSummaries = scala.collection.mutable.Map.empty[EntityID, CharacterSummary]
  private val trapSummaries = scala.collection.mutable.Map.empty[EntityID, TrapSummary]
  private var observers: List[WeakReference[CompendiumRepositoryObserver]] = Nil

  initialize()

  private def initialize() {
    if (dataStore == null) throw new Exception("Failed to open datastore " + dsuri)
    val ents = dataStore.extractEntityData(Set("classid", "base:level", "base:xp", "base:role", "base:class", "base:name", "base:race", "stat:hp"))
    for (ent <- ents) {
      storeSummary(ent._1, ent._2)
    }
  }

  def isStartupComplete = dataStore != null

  protected def storeSummary(eid: EntityID, fields: Map[String, String]) {
    fields.getOrElse("classid", null) match {
      case Compendium.monsterClassIDStorageString =>
        val ent = MonsterSummary.fromFieldMap(eid, fields)
        if (ent != null) monsterSummaries += (eid -> ent)
      case Compendium.characterClassIDStorageString =>
        val ent = CharacterSummary.fromFieldMap(eid, fields)
        if (ent != null) characterSummaries += (eid -> ent)
      case Compendium.trapClassIDStorageString =>
        val ent = TrapSummary.fromFieldMap(eid, fields)
        if (ent != null) trapSummaries += (eid -> ent)
      case s =>
        logger.warn("Can't handle Entity with EntityClassID: " + s)
        logger.warn("Fields:" + fields)
        null
    }
    notifyObservers()
  }

  /**
   * Indicates if the repository has an entity with a give EntityID
   * @param entity Entity to search for
   * @return True is repository has the entity.
   */
  def containsEntity(entity: EntityID) = dataStore.entityExists(entity)

  /**
   * Store entity to the repository.
   */
  def store(entity: CombatantEntity): Boolean = {
    val dse = entity.asDataStoreEntity
    storeSummary(dse.eid, dse.data)
    dataStore.storeEntity(dse)
  }

  /**
   *  Load an CombatantEntity
   * @param eid EntityID to be fetched
   * @param mustBeValid Indicate whether a valid entity is required (true) or not
   * @return The entity if it is found and respects the mustBeValid option
   */
  def load(eid: EntityID, mustBeValid: Boolean): CombatantEntity = {
    val dse = dataStore.loadEntity(eid)
    logger.debug("Loaded Entity {} from datastore, content: {}", eid, dse)

    if (dse != null) {
      if (CombatantEntityBuilder.canHandle(dse)) {
        val ent = CombatantEntityBuilder.buildEntity(dse)
        if (ent != null) {
          logger.debug("Loaded entity, is it valid? {}", ent.isValid)
          ent.dump(logger)
        }
        if (mustBeValid) {
          if (ent != null && ent.isValid) ent else null
        } else ent
      } else {
        logger.warn("Entity loaded with unknown class {}", dse.data.getOrElse("classid", "<no classid found>"))
        null
      }
    } else null
  }

  def delete(eid: EntityID): Boolean = {

    val ret = dataStore.deleteEntity(eid)
    if (ret) {
      logger.debug("Deleted entity {}", eid)
      characterSummaries -= eid
      monsterSummaries -= eid
      trapSummaries -= eid
      notifyObservers()
    } else {
      logger.warn("Failed to delete entity {}", eid)
    }
    ret
  }

  def getMonsterSummaries(): Seq[MonsterSummary] = monsterSummaries.values.toList

  def getCharacterSummaries(): Seq[CharacterSummary] = characterSummaries.values.toList

  def getTrapSummaries(): Seq[TrapSummary] = trapSummaries.values.toList

  def getEntitySummary(eid: EntityID): EntitySummary = {
    if (monsterSummaries.isDefinedAt(eid)) return monsterSummaries(eid)
    if (characterSummaries.isDefinedAt(eid)) return characterSummaries(eid)
    if (trapSummaries.isDefinedAt(eid)) return trapSummaries(eid)
    null
  }

  def registerObserver(obs: CompendiumRepositoryObserver) {
    observers = new WeakReference(obs) :: observers
  }

  private def notifyObservers() {
    observers = observers.map(obs => {
      if (obs.get.isDefined) {
        obs.get.get.compendiumChanged
        obs
      } else {
        null
      }
    }).filter(obs => obs != null)
  }
}
