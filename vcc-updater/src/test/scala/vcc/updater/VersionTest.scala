/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.updater

import org.specs2.SpecificationWithJUnit
import UpdateManager._
import java.io.ByteArrayInputStream
import org.specs2.matcher.DataTables

class VersionTest extends SpecificationWithJUnit with DataTables {

  def is = s2"""
   UpdateManager Version should
     read form string 1.2.3-RC ${
       Version.fromString("1.2.3-RC") must_== Version(1, 2, 3, "RC")
     }
     read form string 1.2.3 ${
       Version.fromString("1.2.3") must_== Version(1, 2, 3, null)
     }
     read form string 1.2 ${
       Version.fromString("1.2") must_== Version(1, 2, 0, null)
     }
     read form string 1.2-SNAPSHOT ${
       Version.fromString("1.2-SNAPSHOT") must_== Version(1, 2, 0, "SNAPSHOT")
     }
     not accept strang string ${
       Version.fromString("foobar") must beNull
     }
     reject non version XML ${
       Version.fromVersionFileFromStream(new ByteArrayInputStream("<va>1.2</va>".getBytes)) must_== UpdateManager.NotFoundVersion
     }
     read from a version file ${
       (Version.fromVersionFileFromStream(this.getClass.getResourceAsStream("/vcc/version.xml")) must not).beNull
     }
     throw exception on strange file ${
       Version.fromVersionFileFromStream(this.getClass.getResourceAsStream("/vcc/Main.class")) must_== UpdateManager.NotFoundVersion
     }

  UpdateManager.Version.eligibleVersion should
    respect eligible update strategy ${ eligible }
  """

  def eligible =
    "from" | "to " | "update allowed" |
      Version(1, 1, 0, null) ! Version(1, 1, 2, null) ! true |
      Version(1, 1, 1, null) ! Version(1, 1, 2, null) ! true |
      Version(1, 1, 1, null) ! Version(1, 2, 0, null) ! true |
      Version(1, 1, 1, null) ! Version(1, 3, 0, null) ! true |
      Version(1, 1, 1, null) ! Version(1, 3, 1, null) ! false |
      Version(1, 1, 1, null) ! Version(1, 3, 0, "RC") ! true |
      Version(1, 1, 1, null) ! Version(1, 3, 1, "RC") ! false |
      Version(1, 1, 1, null) ! Version(1, 3, 0, "SNAPSHOT") ! true |
      //      // Migrate from
      Version(1, 1, 1, "RC") ! Version(1, 1, 1, null) ! true |
      Version(1, 1, 1, "SNAPSHOT") ! Version(1, 1, 1, null) ! true |> {
      (from, to, updatedAllowed) =>
        to.isEligibleUpgradeFromVersion(from) must_== updatedAllowed
    }
}