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

class TemplateDirectiveTest extends SpecificationWithJUnit {
  val fooDS = new MapDataSource(Map("foo" -> "subbar"), Map(), Map())
  val simpleDS = new MapDataSource(Map("foo" -> "bar"), Map("foo" -> List(fooDS)), Map("foo" -> Text("bar")))
  val emptyDS = new MapDataSource(Map(), Map(), Map())
  val engine = new TemplateEngine()
  engine.registerFormatter(FunctionTemplateFormatter("csv", s => s + ", "))

  //Helpers for Resolver
  def resolveEchoData(node: Node, child: NodeSeq) = EchoDataDirective.resolveTemplateNode(node, engine, null, child)

  def resolveIfDefined(node: Node, child: NodeSeq) = IfDefinedDirective.resolveTemplateNode(node, engine, null, child)

  "DataEchoDirective" should {

    "accept only id" in {
      val tn = resolveEchoData(<t:data id="foo"/>, Nil)
      tn must not beNull;
      tn.label must_== "data"
      tn.arguments must_== ("foo", null)
    }

    "accept id and format" in {
      val tn = resolveEchoData(<t:data id="foo" fmt="csv"/>, Nil)
      tn must not beNull;
      tn.arguments._1 must_== "foo"
      tn.arguments._2 must beEqualTo(engine.getFormatter("csv"))
    }

    "fail if no id provided" in {
      resolveEchoData(<t:data ida="foo"/>, Nil) must throwA[IllegalTemplateDirectiveException]
    }

    "fail if fmt not defined" in {
      resolveEchoData(<t:data id="foo" fmt="notfound"/>, Nil) must throwA[IllegalTemplateDirectiveException]
    }

    "render for existing id and format" in {
      val tn = resolveEchoData(<t:data id="foo" fmt="csv"/>, Nil)
      tn.render(simpleDS) must_== NodeSeq.fromSeq(Seq(Text("bar, ")))
    }

    "render for existing id and no format" in {
      val tn = resolveEchoData(<t:data id="foo"/>, Nil)
      tn.render(simpleDS) must_== NodeSeq.fromSeq(Seq(Text("bar")))
    }

    "render for non existant id to Nil" in {
      val tn = resolveEchoData(<t:data id="notfound"/>, Nil)
      tn.render(simpleDS) must_== Nil
    }
  }

  "IfDefinedDirective" should {

    "accept id" in {
      val tn = resolveIfDefined(<t:ifdefined id="foo"/>, Nil)
      tn must not beNull;
      tn.label must_== "ifdefined"
      simpleDS.templateVariable("foo").isDefined must beTrue
      tn.arguments(simpleDS) must beTrue
      tn.arguments(emptyDS) must beFalse
    }

    "accept group" in {
      val tn = resolveIfDefined(<t:ifdefined group="foo"/>, Nil)
      tn must not beNull;
      tn.arguments(simpleDS) must beTrue
      tn.arguments(emptyDS) must beFalse
    }

    "accept styled" in {
      val tn = resolveIfDefined(<t:ifdefined inline="foo"/>, Nil)
      tn must not beNull;
      tn.arguments(simpleDS) must beTrue
      tn.arguments(emptyDS) must beFalse
    }

    "only accept one parameter" in {
      resolveIfDefined(<t:ifdefined id="foo" group="foo"/>, Nil) must throwA[IllegalTemplateDirectiveException]
    }

    "fail if no parameter specified" in {
      resolveIfDefined(<t:ifdefined/>, Nil) must throwA[IllegalTemplateDirectiveException]
    }

    "render for if present" in {
      val tn = resolveIfDefined(<t:ifdefined id="foo">bar</t:ifdefined>, Seq(Text("bar")))
      tn.render(simpleDS) must_== NodeSeq.fromSeq(Seq(Text("bar")))
    }

    "replace child when resolving" in {
      val tn = resolveIfDefined(<t:ifdefined id="foo">bar</t:ifdefined>, Seq(Text("baz")))
      tn.render(simpleDS) must_== NodeSeq.fromSeq(Seq(Text("baz")))
    }

    "no render for if not present" in {
      val tn = resolveIfDefined(<t:ifdefined id="foo">bar</t:ifdefined>, Nil)
      tn.render(emptyDS) must_== Nil
    }
  }

  "render complex nested parsed template" in {
    engine.registerDirective(EchoDataDirective)
    engine.registerDirective(IfDefinedDirective)
    val loader = new TemplateLoader("t",  engine)
    val t = loader.resolveNode(<hey class="nice"><t:ifdefined id="foo"><foo><t:data id="foo"/></foo></t:ifdefined></hey>, null)
    TemplateNode.renderNode(simpleDS, t) must_== (<hey class="nice"><foo>bar</foo></hey>)
  }
}