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
package vcc.domain.dndi

import org.specs._

import Parser._

object XMLParserSpec extends Specification {

  org.apache.log4j.BasicConfigurator.configure();
  
  "parts parser" should {
    "left images from bold " in {
      val ret = Parser.parseBlockElement(<P><BR/><I>Lead <A target="_new" href="80786952472">ref</A>.</I></P>,true)
      ret must_== Block("P#",List(Break(),Emphasis("Lead ref."))) 
    }
    "lift images wrapped in bold" in {
      val xml = (<SPAN class="foo"><B><IMG alt="" src="images/bullet.gif"></IMG></B>Description</SPAN>)
      val ret = Parser.parseBlockElement(xml,true)
      ret must_== Block("SPAN#foo",List(Icon(IconType.Bullet),Text("Description")))
    }
    "handle misplaced publishing information" in {
      val xml = (<SPAN class="foo"><IMG alt="" src="images/bullet.gif"></IMG> Trap.<BR></BR><P><I>First published in <A href="241787400" target="_new">Revenge of the Giants</A>.</I></P></SPAN>)
      val ret = Parser.parseBlockElement(xml,true)
      ret must_== Block("SPAN#foo",List(Icon(IconType.Bullet),Text("Trap."),Break()))
    }
  }
  "Text merger" should {
    "return the last if first is empty" in {
      Text("")+Text("a") must_== Text("a")
    } 
    "return the first if last is empty" in {
      Text("a")+Text("") must_== Text("a")
    }
    "not add spaces if we have a line or space on either side" in {
      Text("a ")+Text("c") must_== Text("a c")
      Text("a\n")+Text("b") must_== Text("a\nb")
      Text("a")+Text(" b") must_== Text("a b")
      Text("a")+Text("\nb") must_== Text("a\nb")
    }
    "add space in other cases" in {
      Text("Abc.")+Text("Cde") must_== Text("Abc. Cde")
      Text("Abc;")+Text("Cde") must_== Text("Abc; Cde")
      Text("Abc")+Text("Cde") must_== Text("Abc Cde")
    }
  }
    
  if(System.getProperty("test.basedir")!= null) {
    val dir = new java.io.File(System.getProperty("test.basedir"))
    "parser" should {
       val dirIter = new vcc.util.DirectoryIterator(dir,false)
       for(file <- dirIter if(file.isFile)) {
         "load "+file in {
        	val xml = scala.xml.XML.loadFile(file)
         val log = org.slf4j.LoggerFactory.getLogger("test")
        	DNDInsiderCapture.getTypeFromXML(xml).isDefined must beTrue
        	DNDInsiderCapture.getIdFromXML(xml).isDefined must beTrue
        	val blocks = parseBlockElements(xml.child,true)
        	blocks must notBeNull
        	blocks must notBeEmpty
            for(b<-blocks) { log.debug("Block:  " + b) }
         }
       }
    }
  }
}
