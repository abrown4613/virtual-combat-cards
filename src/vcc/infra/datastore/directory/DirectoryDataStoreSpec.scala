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

package vcc.infra.datastore.directory

import org.specs._
import org.specs.runner.JUnit4
import vcc.infra.datastore.DataStoreSpec
import vcc.infra.datastore.naming._

class DirectoryDataStoreTest extends JUnit4(DirectoryDataStoreSpec)

object DirectoryDataStoreSpec extends DataStoreSpec {
  def generateStoreID() = {
    val file = new java.io.File(System.getProperty("java.io.tmpdir"),"test"+(new scala.util.Random().nextInt())+".dsd")
    DataStoreURI.fromStorageString("vcc-store:directory:"+ file.toURL.toURI.toString)
  }
  val testStore = new java.io.File("testdata","datastore")
  
  val badTestEntity = Seq("notxml","noid","extra-xml-entity","bad-encoding","missing-datum-id","bad-xml","id-mismatch").map(x=>EntityID.fromName(x))
  
  val testStoreURI = DataStoreURI.fromStorageString("vcc-store:directory:"+testStore.toURL.toURI.toString)
  "DirectoryDataStore on a test directory" ->-(beforeContext {
    storeID = testStoreURI
    val dsb = DataStoreFactory.getDataStoreBuilder(storeID)
    dsb.exists(storeID) must beTrue
    aDataStore = dsb.open(storeID)
    aDataStore must notBeNull
  }) should {
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
}