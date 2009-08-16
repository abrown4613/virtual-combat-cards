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
package test.vcccore.datastoretest

import _root_.vcc.model.datastore._

import java.io.File 

class DirectoryEntityStoreTest extends GenericStoreTest {

  val dir = new java.io.File(System.getProperty("user.home"),"a7s123")
  val storeID = DataStoreURI.asEntityStoreID("vcc-store:directory:"+dir.toURI.toString)
  
  def testReindexFile() {
    loadUpDatabase

    //Close and open again
    close(entityStore)
    new File(dir,"index.toc").delete()
    entityStore = EntityStoreFactory.openStore(storeID)
    assert(entityStore!=null)

    checkEntityExistance(eid0, testClassID)
    val ent = entityStore.load(eid0)
    assert(ent != null)
    assert(ent.id == eid0)
    assert(ent.classId == testClassID)
    checkEntityExistance(eid1, blockClassID)   
  }
  
  def testDirectCreation() {
    if(EntityStoreFactory.exists(storeID)) EntityStoreFactory.getEntityStoreBuilder(storeID).destroy(storeID)
    try {
      val es = new DirectoryEntityStore(storeID)
      assert(false,"Should not get here")
    } catch {
      case s:EntityStoreException => 
      case s => assert(false,"Should not get here" + s)
    }
  }
  
  def testWindowsPaths() {
    val path = new java.io.File(System.getProperties.getProperty("user.home"),"vcc")
    val esid = DataStoreURI.directoryEntityStoreIDFromFile(path)
    assert(esid!=null)
    if(EntityStoreFactory.exists(esid)) {
    	val es = EntityStoreFactory.openStore(esid)
    	assert(es!=null)
    }
  }
  
  def testBadChangeToOnceValidRepository() {
    var base = new File("testdata/badstore1")
    val esid = DataStoreURI.directoryEntityStoreIDFromFile(base)
    assert(esid != null)
    assert(EntityStoreFactory.exists(esid))
    
    //scala.xml.XML.loadFile(new File(base,"bad.xml"))
    //Make sure we rescan
    //new File(base,"index.toc").delete()
    val es = EntityStoreFactory.openStore(esid)
    /*
    val abc = EntityFactory.createInstance(blockClassID,eid0).asInstanceOf[BlockEntity]
    abc.text.value = "abc"
    es.store(abc)*/  
    val ents = es.enumerate(null)
    assert(!ents.isEmpty)
    val sum = es.loadEntitySummary(ents.toSeq(0))
    assert(sum == null, sum)
    try {
    } catch {
      case s:AssertionError => throw s
      case e => assert(false,"Should not get here"+e)
    }
  }

}
