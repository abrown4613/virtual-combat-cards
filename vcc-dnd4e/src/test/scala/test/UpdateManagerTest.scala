/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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
package test

import junit.framework.TestCase
import vcc.util.UpdateManager
import java.net.URL

class UpdateManagerTest extends TestCase {
  
  def testBadURL() {
    try {
    	var x =UpdateManager.checkAvailableVersions(new URL("http://bad.v/ac"))
    	assert(false,"Must have exception")
    } catch {
      case _ => 
    }
  }
  
  def testRealURL() {
    var x =UpdateManager.checkAvailableVersions(new URL("http://www.exnebula.org/files/release-history/vcc/vcc-all.xml"))
    assert(x!=null)
    assert(x.length > 1)
    assert(x(0).version > UpdateManager.Version(0,90,0,null))
    assert(x(0).download.toString.startsWith("http://www.exnebula.org/"))
  }
  
  def testWithLocalFile() {
    val lfile=this.getClass.getClassLoader.getResourceAsStream("test/data/vcc-all.xml")
    val is=new org.xml.sax.InputSource(lfile)
    val x =UpdateManager.checkAvailableVersions(is)
    assert(x!=null)
    assert(x.length > 1)
    assert(x(0).version==UpdateManager.Version(2,0,1,"RC1"))
    assert(x(0).download.toString == "http://www.exnebula.org/files/vcc-2.0.1-RC1.zip")
    assert(x(0).info.toString == "http://www.exnebula.org/node/21")
  }

  def testWithBadLocalFile() {
    val lfile=this.getClass.getClassLoader.getResourceAsStream("test/data/vcc-bad.xml")
    val is=new org.xml.sax.InputSource(lfile)
    try {
      val x =UpdateManager.checkAvailableVersions(is)
      assert(false,"SHould have exception")
    } catch {
      case _ =>
    }
  }
}
