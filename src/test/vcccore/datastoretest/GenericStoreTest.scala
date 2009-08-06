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

import junit.framework.TestCase
import _root_.vcc.model.datastore._

abstract class GenericStoreTest extends TestCase {

  val testClassID = DataStoreURI.asEntityClassID("vcc-class:test")
  val blockClassID = DataStoreURI.asEntityClassID("vcc-class:block")
  val eid0 = DataStoreURI.asEntityID("vcc-ent:monster:0")
  val eid1 = DataStoreURI.asEntityID("vcc-ent:block:1")
  
  class TestEntity(eid:EntityID) extends Entity(eid) {
    val classId = testClassID
    val hp=new IntField(topLevel,"hp")
    val name=new StringField(topLevel,"name")
    val skills=new FieldSet(this,"skills") {
      val perception = new IntField(this,"perception")
      val insight = new IntField(this,"insight")
    }
  }

  class BlockEntity(eid:EntityID) extends Entity(eid) {
    val classId = blockClassID
    val text=new StringField(topLevel,"text")
  }
  
  case class MapEntitySummary(override val eid:EntityID, override val classid:EntityClassID, map:Map[DatumKey,String]) extends EntitySummary(eid,classid)
  object BlockEntityBuilder extends BlockEntity(null) with EntityBuilder {
    def createInstance(eid:EntityID):Entity = new BlockEntity(eid)
    def createSummaryFromMap(eid: EntityID, classid:EntityClassID, fmap:Map[DatumKey,String]):EntitySummary = 
      new MapEntitySummary(eid,classid,fmap)
    val summaryFields = Seq(text.datumKey)
  }
  
  object TestEntityBuilder extends TestEntity(null) with EntityBuilder {
    def createInstance(eid:EntityID):Entity = new TestEntity(eid)
    def createSummaryFromMap(eid: EntityID, classid:EntityClassID, fmap:Map[DatumKey,String]):EntitySummary = 
      new MapEntitySummary(eid,classid,fmap)
    val summaryFields = Seq(name.datumKey,hp.datumKey,skills.perception.datumKey)
  }
  
  def dispose()
  
  def open():EntityStore
  
  def close(es:EntityStore)
  
  var entityStore:EntityStore = null
  
  override def setUp() {
    EntityFactory.registerEntityClass(testClassID, TestEntityBuilder)
    EntityFactory.registerEntityClass(blockClassID, BlockEntityBuilder)
    entityStore = open()
    assert(entityStore != null)
  }
  
  override def tearDown() {
    close(entityStore)
    dispose()
  }
  
  def checkEntityExistance(eid:EntityID,classId:EntityClassID) {
    val ent = entityStore.getEntityClassID(eid)
    assert(Some(classId) == ent, "Entity : "+ent)
    
    var entSet = entityStore.enumerate(null)
    assert(entSet.contains(eid))
    entSet = entityStore.enumerate(classId)
    assert(entSet.contains(eid))
    entSet = entityStore.enumerate(if(classId != blockClassID) blockClassID else testClassID)
    assert(!entSet.contains(eid))
  }
  
  def loadUpDatabase() {
	val ent = EntityFactory.createInstance(testClassID, eid0)
    val et = ent.asInstanceOf[TestEntity]
    
    et.name.value = "Test Entity"
    et.hp.value = 20
    et.skills.perception.value  = 5
    et.skills.insight.value = 6
    
    val block = EntityFactory.createInstance(blockClassID, eid1).asInstanceOf[BlockEntity]
    block.text.value = "A text"
    
    entityStore.store(et)
    checkEntityExistance(et.id, testClassID)
    
    entityStore.store(block)
    checkEntityExistance(block.id, blockClassID)
  }
  
  /**
   * Store two entities and check for their existance
   */
  def testLoadStore() {
    loadUpDatabase
  }
  
  def testPersistance() {
    loadUpDatabase

    //Close and open again
    close(entityStore)
    entityStore = open()

    checkEntityExistance(eid0, testClassID)
    checkEntityExistance(eid1, blockClassID)
    
    val test = entityStore.load(eid0)
    assert(test != null)
    assert(test.id == eid0)
    assert(test.isInstanceOf[TestEntity])
    val tEnt = test.asInstanceOf[TestEntity]
    assert(tEnt.name.value == Some("Test Entity"))

    val block = entityStore.load(eid1)
    assert(block != null)
    assert(block.id == eid1, block.id)
    assert(block.isInstanceOf[BlockEntity],block.classId)
    val tBlock = block.asInstanceOf[BlockEntity]
    assert(tBlock.text.value == Some("A text"))
  }
  
  def testDelete() {
    assert(entityStore.getEntityClassID(eid0) == None)
    loadUpDatabase
    
    //Close and open again

    assert(entityStore.getEntityClassID(eid0) != None)

    entityStore.delete(eid0)
    assert(entityStore.getEntityClassID(eid0) == None)
    checkEntityExistance(eid1, blockClassID)

    //make sure it got saved
    close(entityStore)
    entityStore = open()
    assert(entityStore.getEntityClassID(eid0) == None)
    checkEntityExistance(eid1, blockClassID)
    
  }
  
  def testSummaryMapping() {
    loadUpDatabase
    //Close and open again
    close(entityStore)
    entityStore = open()
    
    assert(entityStore.getEntityClassID(eid0)== Some(testClassID))
    val em = entityStore.loadEntitySummary(eid0)
    assert(em != null)
    assert(em.isInstanceOf[MapEntitySummary])
    val mes = em.asInstanceOf[MapEntitySummary] 
    assert(mes.eid == eid0)
    assert(mes.classid == testClassID)
    assert(!mes.map.isEmpty)
    
    assert(mes.map.contains(TestEntityBuilder.name.datumKey))
    assert(mes.map(TestEntityBuilder.name.datumKey) == "Test Entity")

    assert(mes.map.contains(TestEntityBuilder.hp.datumKey))
    assert(mes.map(TestEntityBuilder.hp.datumKey) == "20")

    assert(mes.map.contains(TestEntityBuilder.skills.perception.datumKey))
    assert(mes.map(TestEntityBuilder.skills.perception.datumKey) == "5")
  }
}
