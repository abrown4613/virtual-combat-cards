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
import org.specs.runner.{JUnit4,JUnitSuiteRunner}
import scala.xml.{Text, Node, NodeSeq}
import org.specs.mock.Mockito

@RunWith(classOf[JUnitSuiteRunner])
class MacroDirectiveTest extends JUnit4(MacroDirectiveSpec)

object MacroDirectiveSpec extends Specification with Mockito {
  val simpleDS = new MapDataSource(Map("foo" -> "bar"), Map(), Map())
  val emptyDS = new MapDataSource(Map(), Map(), Map())
  val engine = new TemplateEngine()
  engine.registerDirective(EchoDataDirective)
  engine.registerDirective(MacroDefineDirective)
  engine.registerDirective(MacroIncludeDirective)
  val loader = new TemplateLoader("t", engine)
  val template = new Template("t")
  val spyTemplate = spy(template)

  //Helpers for Resolver
  def resolveMacroDefine(node: Node, template: Template, child: NodeSeq) = MacroDefineDirective.resolveTemplateNode(node, engine, template, child)

  def resolveMacroInclude(node: Node, template: Template, child: NodeSeq) = MacroIncludeDirective.resolveTemplateNode(node, engine, template, child)

  "MacroDefineDirective" should {

    "accept macro attribute" in {
      val tn = resolveMacroDefine(<t:define macro="foo-macro">bar</t:define>, spyTemplate, Text("bar"))
      tn mustNot beNull
      tn.label must_== "define"
      tn.arguments must_== "foo-macro"
      //Store macros
      there was one(spyTemplate).hasMacro("foo-macro") then
              one(spyTemplate).defineMacro("foo-macro", Text("bar"))
    }

    "reject redefinition of macro attribute" in {
      val mockTemplate = mock[Template]
      mockTemplate.hasMacro("foo-macro") returns true
      val node = <t:define macro="foo-macro">bar</t:define>
      resolveMacroDefine(node, mockTemplate, Text("bar")) must throwAn(new IllegalTemplateDirectiveException("Macro 'foo-macro' is already defined.", node))
      there was one(mockTemplate).hasMacro("foo-macro")
    }

    "exception if more arguments passed" in {
      val node = <t:define macro="foo-macro" fmt="what">bar</t:define>
      resolveMacroDefine(node, template, Nil) must throwAn(new IllegalTemplateDirectiveException("Only 'macro' argument accepted", node))
    }

    "exception if no arguments passed" in {
      val node = <t:define></t:define>
      resolveMacroDefine(node, template, Nil) must throwAn(new IllegalTemplateDirectiveException("Must specify 'macro' argument", node))
    }
  }

  "MacroIncludeDirective" should {

    "accept macro attribute if defined" in {
      val mockTemplate = mock[Template]
      val macroData = <b>wonderbar</b>
      mockTemplate.hasMacro("foo-macro") returns true
      mockTemplate.getMacro("foo-macro") returns macroData
      val tn = resolveMacroInclude(<t:include macro="foo-macro"/>, mockTemplate, Nil)
      tn mustNot beNull
      tn.label must_== "include"
      //Store macros
      there was one(mockTemplate).hasMacro("foo-macro") then
              one(mockTemplate).getMacro("foo-macro")

      tn.arguments._1 must_== "foo-macro"
      tn.arguments._2 must_== macroData
    }

    "reject non define macro" in {
      val mockTemplate = mock[Template]
      val node = <t:include macro="foo-macro"/>
      mockTemplate.hasMacro("foo-macro") returns false
      resolveMacroInclude(node, mockTemplate, Nil) must throwAn(new IllegalTemplateDirectiveException("Macro 'foo-macro' not defined", node))
      there was one(mockTemplate).hasMacro("foo-macro")
    }

    "throw exception if more arguments passed" in {
      val node = <t:include macro="foo-macro" fmt="what"/>
      resolveMacroInclude(node, template, Nil) must throwAn(new IllegalTemplateDirectiveException("Only 'macro' argument accepted", node))
    }

    "throw exception if no arguments passed" in {
      val node = <t:include/>
      resolveMacroInclude(node, template, Nil) must throwAn(new IllegalTemplateDirectiveException("Must specify 'macro' argument", node))
    }

    "throw exception if contains data" in {
      val node = <t:include macro="foo">not allowed</t:include>
      resolveMacroInclude(node, template, Nil) must throwAn(new IllegalTemplateDirectiveException("Directive does does not allow child nodes", node))
    }

  }

  "Macro*Directive rendering with other elemenst" should {
    "render each group accordingly" in {
      val t = loader.resolveNode((<d><t:define macro="foo-macro"><b><t:data id="foo"/></b></t:define><t:include macro="foo-macro"/><t:include macro="foo-macro"/></d>), template)
      TemplateNode.renderNode(simpleDS, t) must_== ( <d><b>bar</b><b>bar</b></d> )
    }
    "not render if groups are empty" in {
      val t = loader.resolveNode((<d><t:define macro="foo-macro"><b><t:data id="foo"/></b></t:define><t:include macro="foo-macro"/><t:include macro="foo-macro"/></d>), template)
      TemplateNode.renderNode(emptyDS, t) must_== ( <d><b></b><b></b></d> )
    }
  }

}