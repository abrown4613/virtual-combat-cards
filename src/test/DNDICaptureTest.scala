//$Id$
package test

import junit.framework.TestCase
import vcc.dndi.Parser

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
     import vcc.dndi.DNDInsiderCapture.flexiToInt
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

    var monster=vcc.dndi.DNDInsiderCapture.load(xml)
    assert(monster!=null)
    assert(monster("NAME")==Some("Death Knight (Human Fighter)"))
    assert(monster("HP")==Some("264"))
  }

  def testIceArchonHailscourge() {
    val file=this.getClass.getClassLoader.getResourceAsStream("test/dndi/data/IceArchonHailscourge.xml")
    assert(file!=null)
    val xml=scala.xml.XML.load(file)
    assert(!xml.isEmpty)

    var monster=vcc.dndi.DNDInsiderCapture.load(xml)
    assert(monster!=null)
    assert(monster("NAME")==Some("Ice Archon Hailscourge"))
    assert(monster("HP")==Some("120"),monster("HP"))
    assert(monster("INITIATIVE")==Some("+11"),monster("INITIATIVE"))
    assert(monster("AC")==Some("30"),monster("AC"))
    assert(monster("REFLEX")==Some("27"),monster("REFLEX"))
  }

}
