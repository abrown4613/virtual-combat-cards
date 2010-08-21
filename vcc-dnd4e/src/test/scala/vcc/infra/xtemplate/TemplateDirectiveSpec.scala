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
import vcc.infra.text.StyledText
import xml.NodeSeq
import scala.xml.{NodeSeq, Text, Node}

@RunWith(classOf[JUnitSuiteRunner])
class TemplateDirectiveTest extends JUnit4(TemplateDirectiveSpec)

class MapDataSource(values: Map[String, String], groups: Map[String, List[TemplateDataSource]], styled: Map[String, StyledText])
        extends TemplateDataSource {
  def group(key: String): List[TemplateDataSource] = groups.getOrElse(key, Nil)

  def getStyledText(key: String): Option[StyledText] = styled.get(key)

  def get(key: String): Option[String] = values.get(key)
}

object TemplateDirectiveSpec extends Specification {
  val fooDS = new MapDataSource(Map("foo" -> "bar"), Map(), Map())
  val simpleDS = new MapDataSource(Map("foo" -> "bar"), Map("foo" -> List(fooDS)), Map("foo" -> StyledText.singleBlock("P", null, "bar")))
  val emptyDS = new MapDataSource(Map(), Map(), Map())

  "DataEchoDirective" should {
    val engine = new TemplateEngine()
    engine.registerFormatter(FunctionTemplateFormatter("csv", s => s + ", "))

    "accept only id" in {
      val tn = EchoDataDirective.resolveTemplateNode(<t:data id="foo"/>, engine, Nil)
      tn mustNot beNull
      tn.label must_== "data"
      tn.arguments must_== ("foo", null)
    }

    "accept id and format" in {
      val tn = EchoDataDirective.resolveTemplateNode(<t:data id="foo" fmt="csv"/>, engine, Nil)
      tn mustNot beNull
      tn.arguments._1 must_== "foo"
      tn.arguments._2 mustEq engine.getFormatter("csv")
    }

    "fail if no id provided" in {
      EchoDataDirective.resolveTemplateNode(<t:data ida="foo"/>, engine, Nil) must throwA[IllegalTemplateDirectiveException]
    }

    "fail if fmt not defined" in {
      EchoDataDirective.resolveTemplateNode(<t:data id="foo" fmt="notfound"/>, engine, Nil) must throwA[IllegalTemplateDirectiveException]
    }

    "render for existing id and format" in {
      val tn = EchoDataDirective.resolveTemplateNode(<t:data id="foo" fmt="csv"/>, engine, Nil)
      tn.render(simpleDS) must_== NodeSeq.fromSeq(Seq(Text("bar, ")))
    }

    "render for existing id and no format" in {
      val tn = EchoDataDirective.resolveTemplateNode(<t:data id="foo"/>, engine, Nil)
      tn.render(simpleDS) must_== NodeSeq.fromSeq(Seq(Text("bar")))
    }

    "render for non existant id to Nil" in {
      val tn = EchoDataDirective.resolveTemplateNode(<t:data id="notfound"/>, engine, Nil)
      tn.render(simpleDS) must_== Nil
    }
  }

  "IfDefinedDirective" should {
    val engine = new TemplateEngine()

    "accept id" in {
      val tn = IfDefinedDirective.resolveTemplateNode(<t:ifdefined id="foo"/>, engine, Nil)
      tn mustNot beNull
      tn.label must_== "ifdefined"
      simpleDS.get("foo").isDefined must beTrue
      tn.arguments(simpleDS) must beTrue
      tn.arguments(emptyDS) must beFalse
    }

    "accept group" in {
      val tn = IfDefinedDirective.resolveTemplateNode(<t:ifdefined group="foo"/>, engine, Nil)
      tn mustNot beNull
      tn.arguments(simpleDS) must beTrue
      tn.arguments(emptyDS) must beFalse
    }

    "accept styled" in {
      val tn = IfDefinedDirective.resolveTemplateNode(<t:ifdefined styled="foo"/>, engine, Nil)
      tn mustNot beNull
      tn.arguments(simpleDS) must beTrue
      tn.arguments(emptyDS) must beFalse
    }

    "only accept one parameter" in {
      IfDefinedDirective.resolveTemplateNode(<t:ifdefined id="foo" group="foo"/>, engine, Nil) must throwA[IllegalTemplateDirectiveException]
    }

    "fail if no parameter specified" in {
      IfDefinedDirective.resolveTemplateNode(<t:ifdefined/>, engine, Nil) must throwA[IllegalTemplateDirectiveException]
    }

    "render for if present" in {
      val tn = IfDefinedDirective.resolveTemplateNode(<t:ifdefined id="foo">bar</t:ifdefined>, engine, Seq(Text("bar")))
      tn.render(simpleDS) must_== NodeSeq.fromSeq(Seq(Text("bar")))
    }

    "replace child when resolving" in {
      val tn = IfDefinedDirective.resolveTemplateNode(<t:ifdefined id="foo">bar</t:ifdefined>, engine, Seq(Text("baz")))
      tn.render(simpleDS) must_== NodeSeq.fromSeq(Seq(Text("baz")))
    }

    "no render for if not present" in {
      val tn = IfDefinedDirective.resolveTemplateNode(<t:ifdefined id="foo">bar</t:ifdefined>, engine, Nil)
      tn.render(emptyDS) must_== Nil
    }
  }

  "render complex nestedt pasrsed template" in {
      val loader = new TemplateLoader("t", TemplateLoaderSpec.engine)
      val t = loader.resolveTemplate(<hey class="nice"><t:ifdefined id="foo"><foo><t:data id="foo"/></foo></t:ifdefined></hey>)
      TemplateUtil.renderNode(simpleDS, t) must_== (<hey class="nice"><foo>bar</foo></hey>)
  }

}