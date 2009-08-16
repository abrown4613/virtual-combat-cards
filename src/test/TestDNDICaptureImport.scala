//$Id$
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
package test

import junit.framework.TestCase
import vcc.app.dndi._
import vcc.domain.dndi._

class TestDNDICaptureImport extends TestCase {

  import scala.xml.{Node,NodeSeq,Text}
  
  var mList:List[Monster] = Nil
  CaptureHoldingArea.addMonsterObserver(new CaptureHoldingArea.CaptureHoldingObserver[Monster] {
	def updateContent(monster:Seq[Monster]) {
	  mList = monster.toList
	}                                          
  })
  
  override def setUp() {
    CaptureHoldingArea.loadCachedMonster()
  }
  
  
  def testBlankTest() {
    
    assert(true)
  }
  
  def testMassImport() {
    val f = new java.io.File(System.getProperty("java.io.tmpdir"),"a.xml")
    for(monster <-mList) {
      println("Monster: "+monster)
      val xml = MonsterStatBlockBuilder.generate(monster)
      assert(xml!=null)
      val pp = new scala.xml.PrettyPrinter(100,2)
      println(pp.formatNodes(xml))
      scala.xml.XML.saveFull(f.toString,xml,"ISO-8859-1",true,null)
    }
  }
}
