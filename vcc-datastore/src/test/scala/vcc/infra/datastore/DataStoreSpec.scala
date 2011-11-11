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
package vcc.infra.datastore

import naming._
import org.specs2.specification.Scope
import org.specs2.mutable.{After, SpecificationWithJUnit}

abstract class DataStoreSpec extends SpecificationWithJUnit {

  args(sequential = true)
  val eid1 = EntityID.fromName("eid:1")
  val eid2 = EntityID.fromName("eid:2")
  val eid3 = EntityID.fromName("eid:3")
  val baseData = Map("name" -> "monster", "xp" -> "100", "block" -> "<a>\n\tother\n\tLine two\n</a>")

  private var partStoreID = Map.empty[Symbol, DataStoreURI]

  def generateStoreID(): DataStoreURI

  def getStoreID(part: Symbol) = {
    if (!partStoreID.isDefinedAt(part)) {
      partStoreID = partStoreID + (part -> generateStoreID())
    }
    partStoreID(part)
  }

  "the DataStoreFactory" should {

    val aStoreID = getStoreID('FactoryStoreID)

    "provide a builder" in {
      val dsb = DataStoreFactory.getDataStoreBuilder(aStoreID)
      dsb must not beNull;
    }
    "must be always the same" in {
      val dsb = DataStoreFactory.getDataStoreBuilder(aStoreID)
      val dsb2 = DataStoreFactory.getDataStoreBuilder(aStoreID)
      dsb2 must not beNull;
      dsb2 must beEqualTo(dsb)
    }
  }

  trait dsbContext extends Scope {
    val storeID = getStoreID('DataStoreBuilder)
    val adsb = DataStoreFactory.getDataStoreBuilder(storeID)
  }

  "a DataStoreBuilder" should {
    "prove the store does not exists before creating" in new dsbContext {
      adsb.exists(storeID) must beFalse
    }
    "not open a store that does not exists" in new dsbContext {
      adsb.open(storeID) must beNull
    }
    "create a store" in new dsbContext {
      adsb.create(storeID) must not beNull;
    }
    "prove it exists after creation" in new dsbContext {
      adsb.exists(storeID) must beTrue
    }
    "open a store that that exists" in new dsbContext {
      adsb.open(storeID) must not beNull;
    }
    "destroy the store" in new dsbContext {
      adsb.destroy(storeID) must beTrue
    }
    "prove the store does not exists after destroying" in new dsbContext {
      adsb.exists(storeID) must beFalse
    }
  }

  trait dataStoreContext extends Scope with After {
    val storeID = getStoreID('DataStoreImplementation)
    val dsb = DataStoreFactory.getDataStoreBuilder(storeID)
    if (!dsb.exists(storeID)) {
      dsb.create(storeID)
    }
    dsb.exists(storeID) must beTrue
    val aDataStore: DataStore = dsb.open(storeID)
    aDataStore must not beNull;

    def after = {
      aDataStore.close()
    }
  }

  "a DataStore implementation" should {

    "be empty after creation" in new dataStoreContext {
      val enum = aDataStore.enumerateEntities
      enum must not beNull;
      enum must_== Nil
      val sets = aDataStore.extractEntityData(Set("name"))
      sets must not beNull;
      sets must_== Nil
    }

    "store an entity" in new dataStoreContext {
      val aDataSet = DataStoreEntity(eid1, baseData)
      val rc = aDataStore.storeEntity(aDataSet)
      rc must beTrue
      val newSet = aDataStore.enumerateEntities()
      newSet must contain(eid1)
      newSet must not contain (eid3);
    }

    "store another entity" in new dataStoreContext {
      val aDataSet = DataStoreEntity(eid2, Map("name" -> "other", "hp" -> "44", "xp" -> "200"))
      val rc = aDataStore.storeEntity(aDataSet)
      rc must beTrue
      val newSet = aDataStore.enumerateEntities()
      newSet must contain(eid2)
      newSet must not contain(eid3);
    }

    "must restore a saved entity" in new dataStoreContext {
      val newSet = aDataStore.enumerateEntities()
      newSet must contain(eid1)
      val ent = aDataStore.loadEntity(eid1)
      ent must not beNull;
      ent.eid must_== eid1
      ent.data must haveTheSameElementsAs(baseData)
    }

    "have non zero time stamp" in new dataStoreContext {
      val ts = aDataStore.entityTimestamp(eid1)
      ts must beGreaterThan[Long](0)
      aDataStore.entityTimestamp(eid3) must beEqualTo[Long](0)
    }

    "inform of the existance of an entity" in new dataStoreContext {
      aDataStore.entityExists(eid1) must beTrue
      aDataStore.entityExists(eid2) must beTrue
      aDataStore.entityExists(eid3) must beFalse
    }

    "update timestamp on a save" in new dataStoreContext {
      val ts1 = aDataStore.entityTimestamp(eid1)
      ts1 must beGreaterThan[Long](0)
      val aDataSet = DataStoreEntity(eid1, Map("name" -> "monster", "xp" -> "101"))
      val rc = aDataStore.storeEntity(aDataSet)
      rc must beTrue
      val ts2 = aDataStore.entityTimestamp(eid1)
      ts2 must beGreaterThan[Long](0)
      ts2 must beGreaterThan(ts1)
    }

    "extract entity data from all entities" in new dataStoreContext {
      val fset = aDataStore.extractEntityData(Set("name", "hp"))
      fset must not beNull;
      fset.size must_== 2
      val map = Map(fset: _*)
      map must haveKey(eid1)
      map must haveKey(eid2)
      map must not be haveKey(eid3)
      map(eid1) must haveTheSameElementsAs(Map("name" -> "monster"))
      map(eid2) must haveTheSameElementsAs(Map("name" -> "other", "hp" -> "44"))
    }

    "remove an entity" in new dataStoreContext {
      aDataStore.deleteEntity(eid1) must beTrue
      aDataStore.entityExists(eid1) must beFalse
    }

    "destroy the DataStore" in new dataStoreContext {
      dsb.destroy(storeID) must beTrue
      dsb.exists(storeID) must beFalse
    }
  }
}