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

package vcc.infra.datastore.naming

import org.specs._
import org.specs.runner.JUnit4
import java.util.UUID

class NamingTest extends JUnit4(NamingSpec)

object NamingSpec extends Specification {
  "Names like EntityID" should {
    "build from UUID" in { 
      val u = UUID.randomUUID
      val e = EntityID(u)
      "and exist and have the correct uuid" in {
        e must_!= null
        e.id must_== u
      }
      "and have proper prefix on storage string" in {
        e.asStorageString must_== "vcc-ent:"+u.toString
      }
    }
    "build from name" in {
      val s = "dndi:monster:123"
      val e = EntityID.fromName(s)
      e must_!= null
      "and match name uuid" in { e.id must_== UUID.nameUUIDFromBytes(s.getBytes)}
    }
    "build from a storage string" in {
      val eid = EntityID.fromName("test this")
      val eid2 = EntityID.fromStorageString(eid.asStorageString)
      eid2 must_!= null
      eid must_== eid2
    }
    "fail to build form a bad string" in {
      EntityID.fromStorageString("test this") must beNull
      EntityID.fromStorageString("vcc-ent:test this") must beNull
    }
  } 
  
}
