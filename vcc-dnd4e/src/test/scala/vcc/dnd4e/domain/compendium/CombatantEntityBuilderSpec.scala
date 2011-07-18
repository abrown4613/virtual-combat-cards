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

package vcc.dnd4e.domain.compendium

import org.junit.runner.RunWith
import org.specs.Specification
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import vcc.infra.datastore.DataStoreEntity
import vcc.infra.datastore.naming.EntityID

@RunWith(classOf[JUnitSuiteRunner])
class CombatantEntityBuilderTest extends JUnit4(CombatantEntityBuilderSpec)

object CombatantEntityBuilderSpec extends Specification {

  val eid = EntityID.generateRandom()

  "CombatantEntityBuilder" should {

    "say it can handle vcc-class:character" in {
      CombatantEntityBuilder.canHandle(new DataStoreEntity(null,Map("classid"->"vcc-class:character"))) must beTrue
    }

    "say it can handle vcc-class:monster" in {
      CombatantEntityBuilder.canHandle(new DataStoreEntity(null,Map("classid"->"vcc-class:monster"))) must beTrue
    }

    "say it cant handle something else" in {
      CombatantEntityBuilder.canHandle(new DataStoreEntity(null,Map("classid"->"something else"))) must beFalse
    }

    "say it cant handle something a strange classid" in {
      CombatantEntityBuilder.canHandle(new DataStoreEntity(null,Map("classid"->"vcc-class:monstra"))) must beFalse
    }

    "return a minimal monster on a build" in {
      val e = CombatantEntityBuilder.buildEntity(new DataStoreEntity(eid,Map("classid"->"vcc-class:monster","base:name"->"me")))
      e mustNot beNull
      e.isInstanceOf[MonsterEntity]
      e.name.value must_== "me"
    }

    "return a minimal monster on a build" in {
      val e = CombatantEntityBuilder.buildEntity(new DataStoreEntity(eid,Map("classid"->"vcc-class:character","base:name"->"me")))
      e mustNot beNull
      e.isInstanceOf[CharacterEntity]
      e.name.value must_== "me"
    }

  }
}