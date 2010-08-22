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
import scala.xml.{Node, NodeSeq, Text}

@RunWith(classOf[JUnitSuiteRunner])
class GroupDirectiveTest extends JUnit4(GroupDirectiveSpec)

object GroupDirectiveSpec extends Specification {
  val foo1DS = new MapDataSource(Map("foo" -> "bar 1"), Map(), Map())
  val foo2DS = new MapDataSource(Map("foo" -> "bar 2"), Map(), Map())
  val simpleDS = new MapDataSource(Map("foo" -> "bar"), Map("foo" -> List(foo1DS, foo2DS)), Map("foo" -> Text("bar")))
  val emptyDS = new MapDataSource(Map(), Map(), Map())
  val engine = new TemplateEngine()
  engine.registerDirective(EchoDataDirective)
  engine.registerDirective(GroupDirective)
  val loader = new TemplateLoader("t", engine)

  //Helpers for Resolver
  def resolveGroup(node: Node, child: NodeSeq) = GroupDirective.resolveTemplateNode(node, engine, null, child)

  "GroupDirective" should {

    "accept ingroup attribute" in {
      val tn = resolveGroup(<t:foreach ingroup="foo"></t:foreach>, Nil)
      tn mustNot beNull
      tn.label must_== "foreach"
      tn.arguments must_== "foo"
    }

    "exception if more argumenst passed" in {
      val node = <t:foreach ingroup="foo" fmt="what"></t:foreach>
      resolveGroup(node, Nil) must throwAn(new IllegalTemplateDirectiveException("Only 'ingroup' argument accepted", node))
    }

    "exception if no argumenst passed" in {
      val node = <t:foreach></t:foreach>
      resolveGroup(node, Nil) must throwAn(new IllegalTemplateDirectiveException("Must specify 'ingroup' argument", node))
    }
  }

  "GroupDirective rendering with other elemenst" should {
    "render each group accordingly" in {
      val t = loader.resolveNode((<ul><t:foreach ingroup="foo"><li><t:data id="foo"/></li></t:foreach></ul>), null)
      TemplateNode.renderNode(simpleDS, t) must_== ( <ul><li>bar 1</li><li>bar 2</li></ul> )
    }
    "not render if groups are empty" in {
      val t = loader.resolveNode((<ul><t:foreach ingroup="foo"><li><t:data id="foo"/></li></t:foreach></ul>), null)
      TemplateNode.renderNode(emptyDS, t) must_== ( <ul></ul> )
    }
  }
}