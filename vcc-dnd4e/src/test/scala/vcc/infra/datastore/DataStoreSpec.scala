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
package vcc.infra.datastore

import org.specs._
import org.specs.runner.JUnit4

import naming._

abstract class DataStoreSpec extends Specification {

  protected var storeID:DataStoreURI = null
  var aDataStore:DataStore = null
  val eid1 = EntityID.fromName("eid:1")
  val eid2 = EntityID.fromName("eid:2")
  val eid3 = EntityID.fromName("eid:3")
  val baseData = Map("name"->"monster","xp"->"100","block"->"<a>\n\tother\n\tLine two\n</a>")
  
  private var partStoreID = Map.empty[Symbol,DataStoreURI]
  
  def generateStoreID():DataStoreURI

  def getStoreID(part:Symbol) = {
    if(!partStoreID.isDefinedAt(part)) {
      partStoreID = partStoreID + (part -> generateStoreID())
    }
    partStoreID(part)
  }
  
  "the DataStoreFactory" should {

    storeID = generateStoreID()
    
    "provide a builder" in {
      val dsb = DataStoreFactory.getDataStoreBuilder(storeID)
      dsb must notBeNull
    }
    "must be always the same" in {
        val dsb = DataStoreFactory.getDataStoreBuilder(storeID)
        val dsb2 = DataStoreFactory.getDataStoreBuilder(storeID)
        dsb2 must notBeNull
        dsb2 must beEqual(dsb)
    }
  }

  //setSequential()

  var adsb:DataStoreBuilder = null

  val dsbContext = beforeContext{
    storeID = getStoreID('DataStoreBuilder)
    adsb = DataStoreFactory.getDataStoreBuilder(storeID)
  }

  "a DataStoreBuilder" ->-(dsbContext)should {
    "prove the store does not exists before creating" in { adsb.exists(storeID) must beFalse }
    "not open a store that does not exists" in { adsb.open(storeID) must beNull }
    "create a store" in { adsb.create(storeID) must notBeNull }
    "prove it exists after creation" in { adsb.exists(storeID) must beTrue }
    "open a store that that exists" in { adsb.open(storeID) must notBeNull }
    "destroy the store" in { adsb.destroy(storeID) must beTrue }
    "prove the store does not exists after destroying" in { adsb.exists(storeID) must beFalse }
  }

  "a DataStore implementation" should {
    doBefore {
      storeID = getStoreID('Store)
      val dsb = DataStoreFactory.getDataStoreBuilder(storeID)
      if(!dsb.exists(storeID)) {
        dsb.create(storeID)
      }
      dsb.exists(storeID) must beTrue
      aDataStore = dsb.open(storeID)
      aDataStore must notBeNull
    }

    doAfter {
	  if(aDataStore!=null) aDataStore.close()
    }
  
    "be empty after creation" in {
      val enum = aDataStore.enumerateEntities
      enum must notBeNull
      enum must_== Nil
      val sets = aDataStore.extractEntityData(Set("name"))
      sets must notBeNull
      sets must_== Nil
    }
    
    "store an entity" in {
      val aDataSet = DataStoreEntity(eid1,baseData)
      val rc = aDataStore.storeEntity(aDataSet)
      rc must beTrue
      val newSet = aDataStore.enumerateEntities()
      newSet must contain (eid1)
      newSet must notContain(eid3)
    }
    
    "store another entity" in {
      val aDataSet = DataStoreEntity(eid2,Map("name"->"other","hp"->"44","xp"->"200"))
      val rc = aDataStore.storeEntity(aDataSet)
      rc must beTrue
      val newSet = aDataStore.enumerateEntities()
      newSet must contain (eid2)
      newSet must notContain(eid3)
    }
    
    "must restore a saved entity" in {
      val newSet = aDataStore.enumerateEntities()
      newSet must contain (eid1)
      val ent = aDataStore.loadEntity(eid1)
      ent must notBeNull
      ent.eid must_== eid1
      ent.data must containAll(baseData)
    }
    
    "have non zero time stamp" in {
      val ts = aDataStore.entityTimestamp(eid1)
      ts must beGreaterThan[Long](0)
      aDataStore.entityTimestamp(eid3) must beEqual[Long](0)
    }
    
    "inform of the existance of an entity" in {
      aDataStore.entityExists(eid1) must beTrue
      aDataStore.entityExists(eid2) must beTrue
      aDataStore.entityExists(eid3) must beFalse
    }
    
    "update timestamp on a save" in {
      val ts1 = aDataStore.entityTimestamp(eid1)
      ts1 must beGreaterThan[Long](0)
      val aDataSet = DataStoreEntity(eid1,Map("name"->"monster","xp"->"101"))
      val rc = aDataStore.storeEntity(aDataSet)
      rc must beTrue
      val ts2 = aDataStore.entityTimestamp(eid1)
      ts2 must beGreaterThan[Long](0)
      ts2 must beGreaterThan(ts1)
    }
    
    "extract entity data from all entities" in {
      val fset = aDataStore.extractEntityData(Set("name","hp"))
      fset must notBeNull
      fset.size must_== 2
      val map = Map(fset: _*)
      map must haveKey(eid1)
      map must haveKey(eid2)
      map must notHaveKey(eid3)
      map(eid1) must containAll(Map("name"->"monster"))
      map(eid2) must containAll(Map("name"->"other","hp"->"44"))
    }
    
    "remove an entity" in {
      aDataStore.deleteEntity(eid1) must beTrue
      aDataStore.entityExists(eid1) must beFalse
    }
    
    "destroy the DataStore" in {
      val dsb = DataStoreFactory.getDataStoreBuilder(storeID)
      dsb.destroy(storeID) must beTrue
      dsb.exists(storeID) must beFalse
    } 
  }
}

