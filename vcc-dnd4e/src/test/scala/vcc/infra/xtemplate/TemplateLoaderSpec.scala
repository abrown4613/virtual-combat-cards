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
import org.specs.mock.Mockito

@RunWith(classOf[JUnitSuiteRunner])
class TemplateLoaderTest extends JUnit4(TemplateLoaderSpec)

object TemplateLoaderSpec extends Specification with Mockito {
  val engine = new TemplateEngine()
  engine.registerDirective(EchoDataDirective)
  engine.registerDirective(IfDefinedDirective)
  val spyEngine = spy(engine)

  "Load plain xml" in {
    val loader = new TemplateLoader("t", engine)
    loader.resolveTemplate(<foo>bar</foo>) must_== <foo>bar</foo>
  }

  "resolve simple directive" in {
    val loader = new TemplateLoader("t", spyEngine)
    val t = loader.resolveTemplate(<foo><t:data id="foo"/></foo>)

    there was one(spyEngine).hasDirective("data")
    t.child(0).isInstanceOf[TemplateNode[_]] must beTrue
  }

  "resolve simple directive in nested xml" in {
    val loader = new TemplateLoader("t", spyEngine)
    val t = loader.resolveTemplate(<hey><foo><t:data id="foo"/></foo></hey>)

    there was one(spyEngine).hasDirective("data")
    val tn = (t \ "foo")(0).child(0)
    tn.isInstanceOf[TemplateNode[_]] must beTrue
  }

  "depth recursion first" in {
    val loader = new TemplateLoader("t", spyEngine)
    val t = loader.resolveTemplate(<hey class="nice"><t:ifdefined id="foo"><foo><t:data id="foo"/></foo></t:ifdefined></hey>)
    there was one(spyEngine).hasDirective("data") then
      one(spyEngine).hasDirective("ifdefined")

    t.child(0).isInstanceOf[TemplateNode[_]] must beTrue
    t.child(0).label must_== "ifdefined"
    t.child(0).child(0).child(0).isInstanceOf[TemplateNode[_]] must beTrue
    t.child(0).child(0).child(0).label must_== "data"

  }

  "surface no directive exception" in {
    val loader = new TemplateLoader("t", spyEngine)
    loader.resolveTemplate(<hey><foo><t:foo id="foo"/></foo></hey>) must
            throwAn(new IllegalTemplateDirectiveException("Directive 'foo' not defined",(<t:foo id="foo"/>)))
  }

  "surface failed exceptions" in {
    val loader = new TemplateLoader("t", spyEngine)
    loader.resolveTemplate(<hey><t:ifdefined id="foo"><foo><t:data ida="foo"/></foo></t:ifdefined></hey>) must
            throwA[IllegalTemplateDirectiveException]
  }
}