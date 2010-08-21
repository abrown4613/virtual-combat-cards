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
//$Id$
package vcc.infra.xtemplate

import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import scala.xml.{NodeSeq, Text, Node}
import vcc.infra.text.StyledText

@RunWith(classOf[JUnitSuiteRunner])
class TemplateTest extends JUnit4(TemplateSpec)

object TemplateSpec extends Specification {

  val fooDS = new MapDataSource(Map("foo"->"bar"), Map(),Map())
  val simpleDS = new MapDataSource(Map("foo"->"bar"), Map("foo"->List(fooDS)),Map("foo"->StyledText.singleBlock("P",null,"bar")))
  val emptyDS = new MapDataSource(Map(), Map(),Map())

  val tmpl = new Template("t")

  "Template.renderNode" should {
    "expand variable" in {
      tmpl.renderNode(simpleDS, <t:data id="foo" />) must_== NodeSeq.fromSeq(Seq(Text("bar")))
    }

    "expand variable not there" in {
      tmpl.renderNode(simpleDS, <t:data id="who" />) must_== Nil
    }

    "expand variable wrapped in non prefixed XML" in {
      tmpl.renderNode(simpleDS, <b><t:data id="foo" /></b>) must_== <b>bar</b>
    }

    "expand variable with whitespace " in {
      tmpl.renderNode(simpleDS, <b> <t:data id="foo" /> </b>) must_== <b> bar </b>
    }

    "expand variable twice" in {
      tmpl.renderNode(simpleDS, <b><t:data id="foo" /><t:data id="foo" /></b>) must_== <b>barbar</b>
    }

    "expand variable in nested non prefixed XML" in {
      tmpl.renderNode(simpleDS, <a><b><c><t:data id="foo" /></c></b></a>) must_== <a><b><c>bar</c></b></a>
    }
  }

  "TemplateUtil.mergeTexts" should {
    "merge text" in {
      TemplateUtil.mergeTexts(NodeSeq.fromSeq(Seq(Text("a"),Text("b")))) must_== NodeSeq.fromSeq(Seq(Text("ab")))
    }
    "preserve XML in the middle" in {
      TemplateUtil.mergeTexts(NodeSeq.fromSeq(Seq[Node](Text("a"),Text("b"),(<b/>),Text("c")))) must_== NodeSeq.fromSeq(Seq[Node](Text("ab"),(<b/>),Text("c")))
    }
  }
}