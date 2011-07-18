/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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

import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4,JUnitSuiteRunner}
import Parser._

@RunWith(classOf[JUnitSuiteRunner])
class ParserTest extends JUnit4(ParserSpec)

object ParserSpec extends Specification {
  "Parser.parseNode" should {
    "parse <B>text<BR/>line<B>" in {
      Parser.parseNode(<B>text<BR/>line</B>) must_== Key("text\nline")
    }

    "parser <B></B>" in {
      Parser.parseNode(<B></B>) must_== Key("")
    }
    "parser <B><IMG/></B>" in {
      //Some really bad HTML here
      Parser.parseNode(<B><IMG/></B>) must_== Key("")
    }
  }

  "parts parser" should {
    "left images from bold " in {
      val ret = Parser.parseBlockElement(<P><BR/><I>Lead <A target="_new" href="80786952472">ref</A>.</I></P>,true)
      ret must_== Block("P#",List(Break(),Emphasis("Lead ref.")))
    }

    "lift images wrapped in bold" in {
      val xml = (<SPAN class="foo"><B><IMG alt="" src="images/bullet.gif"></IMG></B>Description</SPAN>)
      val ret = Parser.parseBlockElement(xml,true)
      ret must_== Block("SPAN#foo",List(Key("[*]"),Text("Description")))
    }
    "handle misplaced publishing information" in {
      val xml = (<SPAN class="foo"><IMG alt="" src="images/bullet.gif"></IMG> Trap.<BR></BR><P><I>First published in <A href="241787400" target="_new">Revenge of the Giants</A>.</I></P></SPAN>)
      val ret = Parser.parseBlockElement(xml,true)
      ret must_== Block("SPAN#foo",List(Icon(IconType.Bullet),Text(" Trap."),Break()))
    }

    "parse empty Bold sections" in {
      // This is found in some traps
      val xml = (<SPAN class="foo"><B>Standard Action</B> <B></B> <BR></BR></SPAN>)
      val ret = Parser.parseBlockElement(xml, true)
      ret must_== Block("SPAN#foo", List(Key("Standard Action"), Text(" "), Key(""), Text(" "), Break()))
    }

    "convert mm3 table to Tabular" in {
      val xml = (<TABLE class="bodytable" xmlns="http://www.w3.org/1999/xhtml"><TBODY><TR><TD><B>HP</B> 220; <B>Bloodied</B> 110</TD><TD class="rightalign"><B>Initiative</B> +7</TD></TR><TR><TD><B>AC</B> 24, <B>Fortitude</B> 22, <B>Reflex</B> 20, <B>Will</B> 22</TD><TD class="rightalign"><B>Perception</B> +15</TD></TR><TR><TD><B>Speed</B> 6</TD><TD class="rightalign">Blindsight 5</TD></TR><TR><TD colspan="2"><B>Resist</B> 5 necrotic</TD></TR><TR><TD colspan="2"><B>Saving Throws</B> +2; <B>Action Points</B> 1</TD></TR></TBODY></TABLE>)
      val ret = Parser.parseBlockElement(xml, true)
      ret must_== Table("bodytable", List(
        Cell(null, List(Key("HP"), Text(" 220; "), Key("Bloodied"), Text(" 110"))),
        Cell("rightalign", List(Key("Initiative"), Text(" +7"))),
        Cell(null, List(Key("AC"), Text(" 24, "), Key("Fortitude"), Text(" 22, "), Key("Reflex"), Text(" 20, "), Key("Will"), Text(" 22"))),
        Cell("rightalign", List(Key("Perception"), Text(" +15"))),
        Cell(null, List(Key("Speed"), Text(" 6"))),
        Cell("rightalign", List(Text("Blindsight 5"))),
        Cell(null, List(Key("Resist"), Text(" 5 necrotic"))),
        Cell(null, List(Key("Saving Throws"), Text(" +2; "), Key("Action Points"), Text(" 1")))))
    }
  }

  "Text merger" should {
    "return the last if first is empty" in {
      Text("") + Text("a") must_== Text("a")
    }
    "return the first if last is empty" in {
      Text("a") + Text("") must_== Text("a")
    }
    "not add spaces if we have a line or space on either side" in {
      Text("a ") + Text("c") must_== Text("a c")
      Text("a\n") + Text("b") must_== Text("a\nb")
      Text("a") + Text(" b") must_== Text("a b")
      Text("a") + Text("\nb") must_== Text("a\nb")
    }
    "add space in other cases" in {
      Text("Abc.") + Text("Cde") must_== Text("Abc. Cde")
      Text("Abc;") + Text("Cde") must_== Text("Abc; Cde")
      Text("Abc") + Text("Cde") must_== Text("Abc Cde")
    }
  }

  if (System.getProperty("test.basedir") != null) {
    val dir = new java.io.File(System.getProperty("test.basedir"))
    "parser" should {
      val dirIter = new vcc.util.DirectoryIterator(dir, false)
      for (file <- dirIter if (file.isFile)) {
        "load " + file in {
          val xml = scala.xml.XML.loadFile(file)
          val log = org.slf4j.LoggerFactory.getLogger("test")
          DNDInsiderCapture.getTypeFromXML(xml).isDefined must beTrue
          DNDInsiderCapture.getIdFromXML(xml).isDefined must beTrue
          val blocks = parseBlockElements(xml.child, true)
          blocks must notBeNull
          blocks must notBeEmpty
          for (b <- blocks) {log.debug("Block:  " + b)}
        }
      }
    }
  }

}