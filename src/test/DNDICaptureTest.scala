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
import vcc.domain.dndi.Parser

class DNDICaptureTest extends TestCase {
  
  
  def testParserBasics() {
    val l=Parser.partsToPairs(Parser.parse((<P class="flavor alt"><B>Equipment</B>: <A href="http://ww2.wizards.com/dnd/insider/item.aspx?fid=6&amp;ftype=2">plate armor</A> .</P>).child))
    assert(l.length==1)
    assert(l.head._1=="Equipment")
    assert(l.head._2=="plate armor.")
    
    val n=Parser.parseNode(scala.xml.Text(" abc; def; "))
    assert(n==Parser.Text("abc; def"),"Found ["+n+"]")
  }
  
  def testLinientToInt {
     import vcc.domain.dndi.DNDInsiderCapture.flexiToInt
     assert(flexiToInt("20")==Some(20))
     assert(flexiToInt(" + 20")==Some(20))
     assert(flexiToInt("+20")==Some(20))
     assert(flexiToInt(" +20")==Some(20))
     assert(flexiToInt(" - 20")==Some(-20))
     assert(flexiToInt(" -20")==Some(-20))
     assert(flexiToInt(" + 20 , ")==Some(20))
     assert(flexiToInt("+20, ")==Some(20))
     assert(flexiToInt(" +20 ;")==Some(20))
     assert(flexiToInt(" - 20 , ")==Some(-20))
     assert(flexiToInt(" -20 ; ")==Some(-20))
     assert(flexiToInt(" sense ; ")==None)
  }

  def testDeathKnightLoad() {
    val file=this.getClass.getClassLoader.getResourceAsStream("test/dndi/data/DeathKnightHumanFighter.xml")
    assert(file!=null)
    val xml=scala.xml.XML.load(file)
    assert(!xml.isEmpty)

    var monster=vcc.domain.dndi.DNDInsiderCapture.load(xml)
    assert(monster!=null)
    assert(monster("NAME")==Some("Death Knight (Human Fighter)"))
    assert(monster("HP")==Some("264"))
  }

  def testIceArchonHailscourge() {
    val file=this.getClass.getClassLoader.getResourceAsStream("test/dndi/data/IceArchonHailscourge.xml")
    assert(file!=null)
    val xml=scala.xml.XML.load(file)
    assert(!xml.isEmpty)

    var monster=vcc.domain.dndi.DNDInsiderCapture.load(xml)
    assert(monster!=null)
    assert(monster("NAME")==Some("Ice Archon Hailscourge"))
    assert(monster("HP")==Some("120"),monster("HP"))
    assert(monster("INITIATIVE")==Some("11"),monster("INITIATIVE"))
    assert(monster("AC")==Some("30"),monster("AC"))
    assert(monster("REFLEX")==Some("27"),monster("REFLEX"))
  }

}
