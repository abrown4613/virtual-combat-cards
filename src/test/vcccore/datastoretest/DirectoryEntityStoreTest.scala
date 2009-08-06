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

  val dir = new java.io.File(System.getProperty("java.io.tmpdir"),"a7s123")
  val storeID = DataStoreURI.asEntityStoreID("vcc-store:directory:"+dir.toURI.toString)
  
  def dispose() {
    if(dir.isDirectory) {
      new vcc.util.DirectoryIterator(dir,false) foreach (x => x.delete)
    }
  }
  
  def open():EntityStore = EntityStoreFactory.createStore(storeID)
  
  def close(es:EntityStore) {
    
  }
  
  def testReindexFile() {
    loadUpDatabase

    //Close and open again
    close(entityStore)
    new File(dir,"index.toc").delete()
    entityStore = open()

    checkEntityExistance(eid0, testClassID)
    val ent = entityStore.load(eid0)
    assert(ent != null)
    assert(ent.id == eid0)
    assert(ent.classId == testClassID)
    checkEntityExistance(eid1, blockClassID)
    
  }
}
