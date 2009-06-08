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

  def testDownload() {
    var file= new java.io.File(System.getProperty("java.io.tmpdir"),"robots.txt")
    file.delete()
    assert(!file.exists)
    val res=UpdateManager.downloadFile(new URL("http://www.exnebula.org/robots.txt"),file)
    assert(res,"Download failed")
    assert(file.exists)
    assert(file.canRead)
    assert(file.isFile)
  }
}
