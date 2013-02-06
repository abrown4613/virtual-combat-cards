/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
import scala.xml.{Node, NodeSeq}

class InlineXMLDirectiveTest extends SpecificationWithJUnit {
  val simpleDS = new MapDataSource(Map(), Map(), Map("foo" -> (<b>bar</b>)))
  val emptyDS = new MapDataSource(Map(), Map(), Map())
  val engine = new TemplateEngine()
  val loader = new TemplateLoader("t", engine)
  engine.registerDirective(InlineXMLDirective)

  //Helpers for Resolver
  def resolveInline(node: Node, child: NodeSeq) = InlineXMLDirective.resolveTemplateNode(node, engine, null, child)

  "InlineDirective" should {

    "accept ingroup attribute" in {
      val tn = resolveInline(<t:inline id="foo" />, Nil)
      tn must not beNull;
      tn.label must_== "inline"
      tn.arguments must_== "foo"
    }

    "exception if more argumenst passed" in {
      val node = <t:inline id="foo" fmt="what" />
      resolveInline(node, Nil) must throwAn(new IllegalTemplateDirectiveException("Only 'id' argument accepted", node))
    }

    "exception if no argumenst passed" in {
      val node = <t:inline />
      resolveInline(node, Nil) must throwAn(new IllegalTemplateDirectiveException("Must specify 'id' argument", node))
    }

    "exception if contains data" in {
      val node = <t:inline id="foo">not allowed</t:inline>
      resolveInline(node, Nil) must throwAn(new IllegalTemplateDirectiveException("Directive does does not allow child nodes", node))
    }
  }

  "GroupDirective rendering with other elemenst" should {
    "render each group accordingly" in {
      val t = loader.resolveNode(<t:inline id="foo" />,null)
      TemplateNode.renderNode(simpleDS, t) must_== (<b>bar</b>)
    }
    "not render if groups are empty" in {
      val t = loader.resolveNode(<t:inline id="foo" />,null)
      TemplateNode.renderNode(emptyDS, t) must_== NodeSeq.Empty
    }
  }
}