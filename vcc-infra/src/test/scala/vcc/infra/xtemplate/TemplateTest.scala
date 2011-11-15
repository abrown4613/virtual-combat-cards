/*
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
package vcc.infra.xtemplate

import org.specs2.mutable.SpecificationWithJUnit
import scala.xml.{NodeSeq, Text, Node}
import org.xml.sax.InputSource
import java.io.StringReader
import org.specs2.specification.Scope

class TemplateTest extends SpecificationWithJUnit {

  trait context extends Scope {
    val fooDS = new MapDataSource(Map("foo" -> "bar"), Map(), Map())
    val simpleDS = new MapDataSource(Map("foo" -> "bar"), Map("foo" -> List(fooDS)), Map("foo" -> Text("bar")))
    val emptyDS = new MapDataSource(Map(), Map(), Map())

    val tmpl = new Template("t")
    val engine = new TemplateEngine()
    engine.registerDirective(EchoDataDirective)
    val loader = new TemplateLoader("t", engine)
    val macro = loader.resolveNode(<t:data id="foo"/>, null).asInstanceOf[TemplateNode[Any]]
    val macro2 = loader.resolveNode(<t:data id="bar"/>, null).asInstanceOf[TemplateNode[Any]]
  }

  "Template" should {

    "define macro" in new context {
      tmpl.defineMacro("macroname", macro)
      tmpl.hasMacro("macroname") must beTrue
    }

    "fecth macro" in new context {
      tmpl.defineMacro("macroname", macro)
      val m: TemplateNode[Any] = tmpl.getMacro("macroname").asInstanceOf[TemplateNode[Any]]
      m must beEqualTo[TemplateNode[Any]](macro)
    }

    "fail to redefine macro" in new context {
      tmpl.defineMacro("macroname", macro)
      tmpl.defineMacro("macroname", macro2) must throwAn[IllegalArgumentException]
    }

    "throw exception while fetching non existant" in new context {
      tmpl.getMacro("notfound").asInstanceOf[TemplateNode[Any]] must throwA[NoSuchElementException]
    }

    "while setting top level node, allow Elem" in new context {
      tmpl.setTopNode(<b>bar</b>)
      tmpl.render(null) must_== <b>bar</b>
    }

    "while setting top level node, reject TemplateNode" in new context {
      tmpl.setTopNode(macro) must throwAn(new IllegalTemplateDirectiveException("Template top level Node must be an Elem", macro))
    }

    "correctly render a loaded template" in new context {
      val t = loader.load(new InputSource(new StringReader("<hey><t:data id='foo' /></hey>")))
      t must not beNull;
      t.prefix must_== "t"
      t.render(simpleDS) must_== <hey>bar</hey>
    }
  }

  "TemplateNode.mergeTexts" should {
    "merge text" in {
      TemplateNode.mergeTexts(NodeSeq.fromSeq(Seq(Text("a"), Text("b")))) must_== NodeSeq.fromSeq(Seq(Text("ab")))
    }
    "preserve XML in the middle" in {
      TemplateNode.mergeTexts(NodeSeq.fromSeq(Seq[Node](Text("a"), Text("b"), (<b/>), Text("c")))) must_== NodeSeq.fromSeq(Seq[Node](Text("ab"), (<b/>), Text("c")))
    }
  }
}