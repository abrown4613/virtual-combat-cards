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
package vcc.infra.datastore.directory

import vcc.infra.datastore.naming._
import java.io.File
import vcc.infra.datastore.{DataStore, DataStoreIOException, DataStoreFactory, DataStoreSpec}

class DirectoryDataStoreTest extends DataStoreSpec {

  shareVariables()
  
  def generateStoreID() = {
    val file = new java.io.File(System.getProperty("java.io.tmpdir"),"test"+(new scala.util.Random().nextInt())+".dsd")
    DataStoreURI.fromStorageString("vcc-store:directory:"+ file.toURI.toString)
  }
  val testStore = new java.io.File("testdata","datastore")
  
  val badTestEntity = Seq("notxml","noid","extra-xml-entity","bad-encoding","missing-datum-id","bad-xml","id-mismatch").map(x=>EntityID.fromName(x))
  
  val testStoreURI = DataStoreURI.fromStorageString("vcc-store:directory:"+testStore.toURI.toString)

  var aDataStore: DataStore = null

  val storeContext = beforeContext {
    val storeID = testStoreURI
    val dsb = DataStoreFactory.getDataStoreBuilder(storeID)
    dsb.exists(storeID) must beTrue
    aDataStore = dsb.open(storeID)
    aDataStore must notBeNull
  }

  "DirectoryDataStore on a test directory" ->-(storeContext) should {
    "have list all bad files" in {
      aDataStore.enumerateEntities() must containAll(badTestEntity)
    }
    for(eid<-badTestEntity) {
      ("process "+(eid.asStorageString)) in { aDataStore.loadEntity(eid) must throwAn[DataStoreIOException] }
    }
    "provide empty field list but not exceptions" in {
      val ef = aDataStore.extractEntityData(Set("type"))
      ef.length must beEqual(0)
      ef must_== Nil
    }
  }

  "Directory DataStoryURI" should {
    "accept absolute URI" in {
      val dsb = DataStoreFactory.getDataStoreBuilder(testStoreURI)
      dsb must notBeNull
      dsb.isResolvedDataStoreURI(testStoreURI) must beTrue
    }
    
    "leave absolute URI unchanged " in {
      val dsb = DataStoreFactory.getDataStoreBuilder(testStoreURI)
      dsb must notBeNull
      dsb.isResolvedDataStoreURI(testStoreURI) must beTrue
      dsb.resolveDataStoreURI(testStoreURI,Map()) must_== testStoreURI
    }
    "replace a variable to become absolute" in {
      val baseURI = DataStoreURI.fromStorageString("vcc-store:directory:file:$VAL/path")
      val repl = (new java.io.File(".")).toURI
      val dsb = DataStoreFactory.getDataStoreBuilder(testStoreURI)
      dsb.isResolvedDataStoreURI(baseURI) must beFalse
      val resolved = dsb.resolveDataStoreURI(baseURI,Map("VAL"->repl))
      resolved must notBeNull
      dsb.isResolvedDataStoreURI(resolved) must beTrue
    }

    "replace a variable to become absolute" in {
      val baseURI = DataStoreURI.fromStorageString("vcc-store:directory:file:$VAL/path/to/some")
      val repl = (new File(System.getProperty("java.io.tmpdir"))).toURI
      val dsb = DataStoreFactory.getDataStoreBuilder(testStoreURI)
      dsb.isResolvedDataStoreURI(baseURI) must beFalse
      val resolved = dsb.resolveDataStoreURI(baseURI,Map("VAL"->repl))
      resolved must notBeNull
      dsb.isResolvedDataStoreURI(resolved) must beTrue
      resolved.uri.toString must_== "vcc-store:directory:"+repl.toString+"path/to/some"
    }

    "replace relative URL with current dir + path" in {
      val baseURI = DataStoreURI.fromStorageString("vcc-store:directory:file:path/to/some.sa")
      val pwd = new File(System.getProperty("user.dir"))
      val dsb = DataStoreFactory.getDataStoreBuilder(testStoreURI)
      dsb.isResolvedDataStoreURI(baseURI) must beFalse
      val resolved = dsb.resolveDataStoreURI(baseURI,Map())
      resolved must notBeNull
      dsb.isResolvedDataStoreURI(resolved) must beTrue
      resolved.uri.toString must_== "vcc-store:directory:"+(new File(pwd,"path/to/some.sa").toURI.toString)
    }
    "must return null if it cant be resolved" in {
      val baseURI = DataStoreURI.fromStorageString("vcc-store:directory:file:$VAL/path/to/some")
      val repl = (new File(".")).toURI
      val dsb = DataStoreFactory.getDataStoreBuilder(testStoreURI)
      dsb.isResolvedDataStoreURI(baseURI) must beFalse
      dsb.resolveDataStoreURI(baseURI,Map("OTHER"->repl)) must beNull
    }
  }
}