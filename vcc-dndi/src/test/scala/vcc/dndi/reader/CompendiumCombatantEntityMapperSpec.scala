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

package vcc.dndi.reader

import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}

@RunWith(classOf[JUnitSuiteRunner])
class CompendiumCombatantEntityMapperTest extends JUnit4(CompendiumCombatantEntityMapperSpec)

/**
 * Service for helping map imported data to Compendium CombatantEntity fields.
 */
object CompendiumCombatantEntityMapperSpec extends Specification {
  "normalize name, role, class" in {
    val names = List("level", "name", "role", "race", "class", "type", "xp")
    for (name <- names) {
      CompendiumCombatantEntityMapper.normalizeCompendiumNames(Map(name -> (name + "-value"))) must_== Map(("base:" + name) -> (name + "-value"))
    }
  }
  "prepend text: to comment" in {
    val names = List("comment", "description")
    for (name <- names) {
      CompendiumCombatantEntityMapper.normalizeCompendiumNames(Map(name -> (name + "-value"))) must_== Map(("text:" + name) -> (name + "-value"))
    }
  }

  "prepend stat: to the others" in {
    val names = List("hey", "what", "is", "this", "test")
    for (name <- names) {
      CompendiumCombatantEntityMapper.normalizeCompendiumNames(Map(name -> (name + "-value"))) must_== Map(("stat:" + name) -> (name + "-value"))
    }
  }
}