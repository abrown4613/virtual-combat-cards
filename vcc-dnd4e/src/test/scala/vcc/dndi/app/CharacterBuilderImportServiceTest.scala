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
package vcc.dndi.app

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import java.io.InputStream
import vcc.dnd4e.compendium.{CharacterEntity, CombatantEntityBuilder}
import vcc.dndi.reader.CharacterBuilderImporter

class CharacterBuilderImportServiceTest extends SpecificationWithJUnit {

  trait someCharacter extends Scope {
    val inputStream: InputStream = this.getClass.getResourceAsStream("/vcc/dndi/app/Fionn.xml")
  }

  "return entity must be loadable into a CharacterEntity" in new someCharacter {
    val dse = CharacterBuilderImporter.loadFromStream(inputStream)
    val ent = CombatantEntityBuilder.buildEntity(dse)
    (ent must not beNull)
    ent.isInstanceOf[CharacterEntity] must beTrue
    ent.asInstanceOf[CharacterEntity].name.value must_== "Fionn"
  }
}