/**
 * Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.util

import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import vcc.util.UpdateManager.Version
import java.io.ByteArrayInputStream

@RunWith(classOf[JUnitSuiteRunner])
class UpdateManagerTest extends JUnit4(UpdateManagerSpec)

object UpdateManagerSpec extends Specification {
  "UpdateManager Version" should {
    "read form string 1.2.3-RC" in {
      Version.fromString("1.2.3-RC") must_== Version(1,2,3,"RC")
    }

    "read form string 1.2.3" in {
      Version.fromString("1.2.3") must_== Version(1,2,3,null)
    }

    "read form string 1.2" in {
      Version.fromString("1.2") must_== Version(1,2,0,null)
    }

    "read form string 1.2-SNAPSHOT" in {
      Version.fromString("1.2-SNAPSHOT") must_== Version(1,2,0,"SNAPSHOT")
    }

    "not accept strang string" in {
      Version.fromString("foobar") must beNull
    }

    "reject non version XML" in {
      Version.fromVersionFileFromStream(new ByteArrayInputStream("<va>1.2</va>".getBytes)) must beNull    
    }

    "read from a version file" in {
      Version.fromVersionFileFromStream(this.getClass.getResourceAsStream("/vcc/version.xml")) mustNot beNull
    }

    "throw exception on strange file" in {
      Version.fromVersionFileFromStream(this.getClass.getResourceAsStream("/vcc/Main.class")) must beNull
    }
  }
}