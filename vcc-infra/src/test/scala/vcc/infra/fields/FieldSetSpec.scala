/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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

package vcc.infra.fields

import org.specs._
import org.specs.runner.JUnit4

import vcc.infra.datastore.DataStoreEntity
import vcc.infra.datastore.naming._

class FieldSetTest extends JUnit4(FieldSetSpec)

object FieldSetSpec extends Specification {
  
  class MyFieldSet extends FieldSet(EntityID.fromName("mytest")) {
    val number = new IntField(this,"attr:number",new DefaultIntFieldValidator())
    val str = new StringField(this,"attr:string",new DefaultStringFieldValidator())
  }
  
  
  "a FieldSet " should {
  
	var fieldSet:MyFieldSet = null
  
    doBefore {
      fieldSet = new MyFieldSet()
    }
    
    "start of all undefined" in {
      fieldSet must notBeNull
      fieldSet.number.isDefined must beFalse
      fieldSet.str.isDefined must beFalse
    }
    
    "map of empty set is empty" in {
      val dse = fieldSet.asDataStoreEntity()
      dse.data must beEmpty
    }
    
    "provide the correct values as an DataStoreEntity" in {
      fieldSet.number.value = 10
      fieldSet.str.value = "a string"
      val dse = fieldSet.asDataStoreEntity()
      dse must notBeNull
      dse.data must havePair ("attr:number" -> "10")
      dse.data must havePair ("attr:string" -> "a string")
    }                                                      
    
    "load from a map of fields" in { 
      val map = Map("attr:number" -> "10", "attr:string" -> "a string" )
      fieldSet.loadFromMap(map) must beTrue
      fieldSet.number.isDefined must beTrue
      fieldSet.number.value must_== 10
      fieldSet.str.isDefined must beTrue
      fieldSet.str.value must_== "a string"
    }

    "load from a map of fields with a bad field" in { 
      val map = Map("attr:number" -> "not a number", "attr:string" -> "a string" )
      fieldSet.loadFromMap(map) must beFalse
      fieldSet.number.isDefined must beFalse
      fieldSet.number.isValid must beFalse
      fieldSet.str.isDefined must beTrue
      fieldSet.str.value must_== "a string"
    }
    
  }
}
