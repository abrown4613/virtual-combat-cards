/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package test

import vcc.util.UpdateManager
import java.net.URL
import org.junit.Test
import org.xml.sax.InputSource

class UpdateManagerTest {

  @Test(expected = classOf[Exception])
  def testBadURL() {
    UpdateManager.checkAvailableVersions(new URL("http://bad.v/ac"))
  }

  @Test
  def testRealURL() {
    val releases = UpdateManager.checkAvailableVersions(new URL("http://www.exnebula.org/files/release-history/vcc/vcc-all.xml"))
    assert(releases != null)
    assert(releases.length > 1)
    assert(releases(0).version > UpdateManager.Version(0, 90, 0, null))
    assert(releases(0).download.toString.startsWith("http://www.exnebula.org/"))
  }

  @Test
  def testWithLocalFile() {
    val releases = UpdateManager.checkAvailableVersions(contentFromStream("test/data/vcc-all.xml"))
    assert(releases != null)
    assert(releases.length > 1)
    assert(releases(0).version == UpdateManager.Version(2, 0, 1, "RC1"))
    assert(releases(0).download.toString == "http://www.exnebula.org/files/vcc-2.0.1-RC1.zip")
    assert(releases(0).info.toString == "http://www.exnebula.org/node/21")
  }

  @Test(expected = classOf[Exception])
  def testWithBadLocalFile() {
    UpdateManager.checkAvailableVersions(contentFromStream("test/data/vcc-bad.xml"))
  }

  def contentFromStream(resourcePath: String):InputSource = {
    new InputSource(this.getClass.getClassLoader.getResourceAsStream(resourcePath))
  }
}