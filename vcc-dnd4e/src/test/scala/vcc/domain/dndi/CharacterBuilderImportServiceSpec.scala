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
package vcc.domain.dndi

import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnitSuiteRunner, JUnit4}
import scala.xml.{XML}
import java.io.InputStream
import org.xml.sax.InputSource
import vcc.infra.datastore.naming.EntityID
import vcc.infra.datastore.{DataStoreIOException}
import vcc.dnd4e.domain.compendium.{CharacterEntity, CombatantEntityBuilder}

@RunWith(classOf[JUnitSuiteRunner])
class CharacterBuilderImportServiceTest extends JUnit4(CharacterBuilderImportServiceSpec)

object CharacterBuilderImportServiceSpec extends Specification {
  var is: InputStream = null

  "CharacterBuilderObject parsing" should {
    "accept multiple aliases" in {
      val stat = (<Stat value="17">
          <alias name="AC"/>
          <alias name="Armor Class"/>
          <statadd Level="1" value="10" charelem="18177d40"/>
          <statadd Level="1" value="1" statlink="HALF-LEVEL" charelem="18177d40"/>
          <statadd type="Ability" Level="1" not-wearing="armor:heavy" value="1" statlink="Dexterity" abilmod="true" charelem="18177d40"/>
          <statadd type="Ability" Level="1" not-wearing="armor:heavy" value="1" statlink="Intelligence" abilmod="true" charelem="18177d40"/>
          <statadd type="Defensive" Level="1" wearing="DEFENSIVE:" value="1" charelem="18177d40"/>
          <statadd type="Armor" Level="2" value="6" charelem="1817a218"/>
      </Stat>)

      val sm = CharacterBuilderObject.extractStat(stat)
      sm must notBeEmpty
      sm must contain("ac", 17)
      sm must contain("armor class", 17)
    }
  }


  val fionn = beforeContext {
    is = this.getClass.getResourceAsStream("/vcc/domain/dndi/Fionn.xml")
  }

  "context setup" ->- (fionn) should {
    "point to a valid XML" in {
      is mustNot beNull
      val elem = XML.load(new InputSource(is))
      elem mustNot beNull
    }
  }

  "Import Service with valid file" ->- (fionn) should {
    "return a valid DataStoreEntity" in {
      val dse = CharacterBuilderImporter.loadFromStream(is)
      dse mustNot beNull
    }
    "bring a valid entityid" in {
      val dse = CharacterBuilderImporter.loadFromStream(is)
      dse.eid mustNot beNull
      dse.eid must_== EntityID.fromName("dndi:character:Fionn:8")
    }

    "return entity with correct classid" in {
      val dse = CharacterBuilderImporter.loadFromStream(is)
      dse.data("classid") must_== "vcc-class:character"
    }

    "return entity with valid name" in {
      val dse = CharacterBuilderImporter.loadFromStream(is)
      dse.data("base:name") must_== "Fionn"
    }

    "return entity with valid level" in {
      val dse = CharacterBuilderImporter.loadFromStream(is)
      dse.data("base:level") must_== "8"
    }

    "return entity with valid class" in {
      val dse = CharacterBuilderImporter.loadFromStream(is)
      dse.data("base:class") must_== "Rogue"
    }

    "return entity with valid class" in {
      val dse = CharacterBuilderImporter.loadFromStream(is)
      dse.data("base:race") must_== "Elf"
    }

    "return entity with sense" in {
      val dse = CharacterBuilderImporter.loadFromStream(is)
      dse.data("base:senses") must_== "Low-light"
    }

    "return base skills " in {
      val expect = Map(
        "skill:insight" -> "12",
        "skill:perception" -> "14")
      val dse = CharacterBuilderImporter.loadFromStream(is)
      dse.data must containAll(expect)
    }

    "return all required stats:*" in {
      val expect = Map(
        "stat:ac" -> "22",
        "stat:fortitude" -> "17",
        "stat:hp" -> "59",
        "stat:initiative" -> "8",
        "stat:reflex" -> "22",
        "stat:will" -> "19")
      val dse = CharacterBuilderImporter.loadFromStream(is)
      dse.data must containAll(expect)
    }

    "return entity must be loadable into a CharacterEntity" in {
      val dse = CharacterBuilderImporter.loadFromStream(is)
      val ent = CombatantEntityBuilder.buildEntity(dse)
      ent mustNot beNull
      ent.isInstanceOf[CharacterEntity] must beTrue
    }
  }

  "with invalid files" should {
    "return exception on a non XML file" in {
      val is: InputStream = new java.io.ByteArrayInputStream("bad file".getBytes())
      CharacterBuilderImporter.loadFromStream(is) must throwA(new DataStoreIOException("Invalid XML file", null))
    }

    "return exception on a XML file of other type" in {
      val is: InputStream = new java.io.ByteArrayInputStream("<?xml version='1.0' ?><xml></xml>".getBytes())
      CharacterBuilderImporter.loadFromStream(is) must throwA(new DataStoreIOException("XML file does not represent a DND4E file", null))
    }

    "return exception on a XML incomplete DND4e file" in {
      val is: InputStream = new java.io.ByteArrayInputStream("<D20Character game-system=\"D&amp;D4E\" Version=\"0.07a\" legality=\"rules-legal\"></D20Character>".getBytes())
      CharacterBuilderImporter.loadFromStream(is) must throwA(new DataStoreIOException("Incomplete DND4E file", null))
    }
  }
}
