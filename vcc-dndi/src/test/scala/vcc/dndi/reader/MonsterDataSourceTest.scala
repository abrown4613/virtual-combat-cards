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
package vcc.dndi.reader

import vcc.infra.xtemplate.TemplateDataSource
import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito

class MonsterDataSourceTest extends SpecificationWithJUnit with Mockito {

  "Monster as DataSource" should {

    val mockLegacy = List(mock[Power])
    val mockPowerGroup = List(mock[Power])
    val mockAttribute = mock[Map[String, String]]
    val monster = new Monster(10, mockAttribute, mockLegacy, Map(ActionType.Standard -> mockPowerGroup))

    "return an attribute" in {
      //Should be case insensitive
      mockAttribute.get("foo") returns Some("bar")
      monster.templateVariable("FoO") must_== Some("bar")
      there was one(mockAttribute).get("foo")
    }

    "return legacy group" in {
      monster.templateGroup("legacy") must beEqualTo(mockLegacy.asInstanceOf[List[TemplateDataSource]])
    }

    "return action group by action name group" in {
      ActionType.unapply("staNdard action") must_== Some(ActionType.Standard) // Safety for sanity
      monster.templateGroup("staNdard action") must beEqualTo[List[TemplateDataSource]](mockPowerGroup)
    }
  }
}
