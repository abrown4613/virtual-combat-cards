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
package vcc.infra.fields

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import vcc.infra.datastore.naming._

class FieldSetTest extends SpecificationWithJUnit {

  class MyFieldSet extends FieldSet(EntityID.fromName("mytest")) {
    val number = new IntField(this, "attr:number", new DefaultIntFieldValidator())
    val str = new StringField(this, "attr:string", new DefaultStringFieldValidator())
  }

  trait context extends Scope {
    val fieldSet: MyFieldSet = new MyFieldSet()
  }

  "a FieldSet " should {
    "start of all undefined" in new context {
      fieldSet must not beNull;
      fieldSet.number.isDefined must beFalse
      fieldSet.str.isDefined must beFalse
    }

    "map of empty set is empty" in new context {
      val dse = fieldSet.asDataStoreEntity()
      dse.data must beEmpty
    }

    "provide the correct values as an DataStoreEntity" in new context {
      fieldSet.number.value = 10
      fieldSet.str.value = "a string"
      val dse = fieldSet.asDataStoreEntity()
      dse must not beNull;
      dse.data must havePair("attr:number" -> "10")
      dse.data must havePair("attr:string" -> "a string")
    }

    "load from a map of fields" in new context {
      val map = Map("attr:number" -> "10", "attr:string" -> "a string")
      fieldSet.loadFromMap(map) must beTrue
      fieldSet.number.isDefined must beTrue
      fieldSet.number.value must_== 10
      fieldSet.str.isDefined must beTrue
      fieldSet.str.value must_== "a string"
    }

    "load from a map of fields with a bad field" in new context {
      val map = Map("attr:number" -> "not a number", "attr:string" -> "a string")
      fieldSet.loadFromMap(map) must beFalse
      fieldSet.number.isDefined must beFalse
      fieldSet.number.isValid must beFalse
      fieldSet.str.isDefined must beTrue
      fieldSet.str.value must_== "a string"
    }
  }
}
