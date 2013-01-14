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
import org.specs2.mock.Mockito
import java.io.StringReader
import org.xml.sax.InputSource

class TemplateLoaderTest extends SpecificationWithJUnit with Mockito {
  val engine = new TemplateEngine()
  engine.registerDirective(EchoDataDirective)
  engine.registerDirective(IfDefinedDirective)
  val template = new Template("t")

  "Load plain xml" in {
    val loader = new TemplateLoader("t", engine)
    loader.resolveNode(<foo>bar</foo>, template) must_== <foo>bar</foo>
  }

  "resolve simple directive" in {
    val spyEngine = spy(engine)
    val loader = new TemplateLoader("t", spyEngine)
    val t = loader.resolveNode(<foo><t:data id="foo"/></foo>, template)

    there was one(spyEngine).hasDirective("data")
    t.child(0).isInstanceOf[TemplateNode[_]] must beTrue
  }

  "resolve simple directive in nested xml" in {
    val spyEngine = spy(engine)
    val loader = new TemplateLoader("t", spyEngine)
    val t = loader.resolveNode(<hey><foo><t:data id="foo"/></foo></hey>, template)

    there was one(spyEngine).hasDirective("data")
    val tn = (t \ "foo")(0).child(0)
    tn.isInstanceOf[TemplateNode[_]] must beTrue
  }

  "depth recursion first" in {
    val spyEngine = spy(engine)
    val loader = new TemplateLoader("t", spyEngine)
    val t = loader.resolveNode(<hey class="nice"><t:ifdefined id="foo"><foo><t:data id="foo"/></foo></t:ifdefined></hey>, template)
    there was one(spyEngine).hasDirective("data") andThen
      one(spyEngine).hasDirective("ifdefined")

    t.child(0).isInstanceOf[TemplateNode[_]] must beTrue
    t.child(0).label must_== "ifdefined"
    t.child(0).child(0).child(0).isInstanceOf[TemplateNode[_]] must beTrue
    t.child(0).child(0).child(0).label must_== "data"

  }

  "surface no directive exception" in {
    val spyEngine = spy(engine)
    val loader = new TemplateLoader("t", spyEngine)
    loader.resolveNode(<hey><foo><t:foo id="foo"/></foo></hey>, template) must
            throwAn(new IllegalTemplateDirectiveException("Directive 'foo' not defined",(<t:foo id="foo"/>)))
  }

  "surface failed exceptions" in {
    val spyEngine = spy(engine)
    val loader = new TemplateLoader("t", spyEngine)
    loader.resolveNode(<hey><t:ifdefined id="foo"><foo><t:data ida="foo"/></foo></t:ifdefined></hey>, template) must
            throwA[IllegalTemplateDirectiveException]
  }

  "throw exception on null inputsource" in {
    val loader = new TemplateLoader("t", engine)
    loader.load(null) must throwA[IllegalArgumentException]
  }

  "load from inputsource" in {
    val loader = new TemplateLoader("t", engine)
    val t = loader.load(new InputSource(new StringReader("<hey><t:data id='foo' /></hey>")))
    t must not beNull;
    t.prefix must_== "t"
  }

  "throw exception if top level element of XML is not a Elem" in {
    val loader = new TemplateLoader("t", engine)
    loader.load(new InputSource(new StringReader("<t:data id='foo' />"))) must throwAn[IllegalTemplateDirectiveException]
  }.pendingUntilFixed("2.10 change")
}