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
//$Id$
package test

import junit.framework.TestCase

import vcc.model.datastore._

class DataStoreTest extends TestCase {

  object Ctype extends Enumeration {
    val alpha=Value("Alpha")
    val beta=Value("Beta")
  }
  
  class TestEntity(id:String) extends Entity(id) {
    
    val classId="class:data"
    
    val ctype=new EnumerationField(topLevel,"ctype",Ctype)
    val hp=new IntField(topLevel,"hp")
    val name=new StringField(topLevel,"name")
    val skills=new FieldSet(this,"skills") {
      val perception = new IntField(this,"perception")
      val insight = new IntField(this,"insight")
    }

    val powers = new MultiSet[Power](this,"powers",()=>{ new Power()})
    
    class Power extends MultiSetFieldContainer(powers) {
      val name = new StringField(this,"name")
      val atk = new IntField(this,"attack")
      val damage= new StringField(this,"damage")
    }
    
  }
  
  override def setUp {
    EntityFactory.registerEntityClass("class:data", id => new TestEntity(id))
  }
  
  /**
   * Check entity EntityFactory functions  
   */
  def testSanity() {
    assert(EntityFactory.isClassDefined("class:data"))
    val ent=EntityFactory.createInstance("class:data","data:0")
    assert(ent!=null)
    assert(ent.isInstanceOf[Entity])
  }
  
  def testLoadFromBadDummyNoField {
    val src= new ListEntitySource("class:data","test:0", Seq(Datum("base",0,"nofield","Test")))
    
    try { 
      val ent=EntityLoader.load(src)
      assert(false,"Should not get here")
    } catch {
      case e: UnexistantField => assert(true)
      case s => assert(false,"Unexpected exception:"+s)
    }
  }

  def testLoadFromBadDummyNoFieldSet {
    val src= new ListEntitySource("class:data","test:0", Seq(Datum("nobase",0,"nofield","Test")))
    
    try { 
      val ent=EntityLoader.load(src)
      assert(false,"Should not get here")
    } catch {
      case e: UnexistantField => assert(true)
      case s => assert(false,"Unexpected exception:"+s)
    }
  }

  def testLoadFromBadFieldInMultiSet {
    val src= new ListEntitySource("class:data","test:0", Seq(Datum("powers",0,"nofield","Test")))
    
    try { 
      val ent=EntityLoader.load(src)
      assert(false,"Should not get here")
    } catch {
      case e: UnexistantField => assert(true)
      case s => assert(false,"Unexpected exception:"+s)
    }
  }

  def testLoadFromBadIntField {
    val src= new ListEntitySource("class:data","test:0", Seq(Datum("base",0,"hp","Test")))
    
    try { 
      val ent=EntityLoader.load(src)
      assert(false,"Should not get here")
    } catch {
      case e: NumberFormatException => assert(true)
      case s => assert(false,"Unexpected exception:"+s)
    }
  }
  
  def testLoadFromBadEnumerationField {
    val src= new ListEntitySource("class:data","test:0", Seq(Datum("base",0,"ctype","Test")))
    
    try { 
      val ent=EntityLoader.load(src)
      assert(false,"Should not get here")
    } catch {
      case e: EnumerationConstantNotPresentException => assert(true)
      case s => assert(false,"Unexpected exception:"+s)
    }
  }
  
  def testLoadFromDummySource {
    val src= new ListEntitySource("class:data","test:0", Seq(
        Datum("base",0,"name","Test"),
        Datum("base",0,"ctype","Alpha"),
        Datum("powers",0,"name","Test Power"),
        Datum("powers",1,"name","Another Power"),
        Datum("skills",0,"perception","10")
      ))
 
    val ent=EntityLoader.load(src)
    assert(ent!=null)
    assert(ent.id=="test:0")
    assert(ent.classId=="class:data")
    assert(ent.isInstanceOf[TestEntity])
    val et=ent.asInstanceOf[TestEntity]
    assert(et.name.value==Some("Test"),et.name.value)
    assert(et.ctype.value==Some(Ctype.alpha),et.ctype.value)
    assert(et.skills.perception.value==Some(10))
    assert(et.powers(0).name.value==Some("Test Power"))
    assert(et.powers(0).atk.value==None)
    assert(et.powers(1).name.value==Some("Another Power"))
  }
  
  def testNewEntity() {
    var v=new TestEntity("vcc:data:2")
    
    v.hp.value=10
    v.name.value="Goblin"
    v.skills.perception.value=15
    
    var idx=v.powers.addInstance()
    v.powers(idx).name.value="Smack it"
    v.powers(idx).atk.value=10

    idx=v.powers.addInstance()
    v.powers(idx).name.value="Blast it"
    v.powers(idx).atk.value=8
    v.powers(idx).damage.value = "2d8+3"
    assert(v.powers(idx).name.value==Some("Blast it"), v.powers(idx).name)
    assert(v.powers(idx).atk.value==Some(8))
    assert(v.powers(idx).damage.value == Some("2d8+3"))
  }
  
  def testXMLLoad() {
    val xml = <entity classId="class:data" id="vcc:data:1">
      <set id="base">
        <datum id="hp">40</datum>
        <datum id="ctype">Alpha</datum>
        <datum id="name">Goblin</datum>
      </set>
      <set id="skills">
        <datum id="perception">15</datum>
      </set>
      <mset id="powers">
        <set id="0">
          <datum id="name">Smack it</datum>
          <datum id="attack">10</datum>
        </set>
        <set id="1">
          <datum id="name">Blast it</datum>
          <datum id="attack">8</datum>
          <datum id="damage">2d8+3</datum>
        </set>
      </mset>
    </entity>

	assert(xml!=null)
	val ds=EntityXMLFileLoader.dataFromXML(xml)
	assert(ds!=null)
	val absEnt=EntityLoader.load(ds)
	assert(absEnt!=null)
    assert(absEnt.isInstanceOf[TestEntity])
    val ent=absEnt.asInstanceOf[TestEntity]
 
    assert(ent.name.value==Some("Goblin"),ent.name.value)
    assert(ent.hp.value==Some(40),ent.hp.value)
    assert(ent.ctype.value==Some(Ctype.alpha),ent.ctype.value)
    assert(ent.skills.perception.value==Some(15))
    assert(ent.skills.insight.value==None)
    
    assert(ent.powers(0).name.value==Some("Smack it"))
    assert(ent.powers(0).atk.value==Some(10))
    assert(ent.powers(0).damage.value==None)
    
    assert(ent.powers(1).name.value==Some("Blast it"))
    assert(ent.powers(1).atk.value==Some(8))
    assert(ent.powers(1).damage.value==Some("2d8+3"))
  }
  
  def testBadXML() {
    val xmls:List[(String,scala.xml.Node)]= List(
      ("Not entity", <abc id="an"></abc>),
      ("No Classid", <entity></entity>),
      ("No ID", <entity classId="abc"></entity>),
      ("No set ID", <entity classId="abc" id="ent"><set></set></entity>),
      ("No datum ID", <entity classId="abc" id="ent"><set id="base"><datum /></set></entity>),
      ("No mset ID", <entity classId="abc" id="ent"><mset></mset></entity>),
      ("No mset/set ID", <entity classId="abc" id="ent"><mset id="mset"><set></set></mset></entity>),
      ("Bad mset/set ID", <entity classId="abc" id="ent"><mset id="mset"><set id="a"></set></mset></entity>),
      ("Bad mset/set field id", <entity classId="abc" id="ent"><mset id="mset"><set id="1"><datum /></set></mset></entity>)
    )
    for(xml<-xmls) {
      try {
	    val ds=EntityXMLFileLoader.dataFromXML(xml._2)
	    assert(false,"Exception expected case: "+xml._1)
      } catch {
      	case e:InvalidEntityXMLException =>
      	case ae: AssertionError => throw ae
      	case s => assert(false,"Unexcepted exception "+s +" in case "+xml._1)
      }
    }
  }
  
}

