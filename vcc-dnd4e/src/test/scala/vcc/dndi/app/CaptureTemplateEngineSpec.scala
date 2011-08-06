/**
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
package vcc.dndi.app

import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import vcc.dnd4e.Configuration
import java.io.File
import vcc.infra.xtemplate.Template
import vcc.infra.diskcache.FileUpdateAwareLoader

@RunWith(classOf[JUnitSuiteRunner])
class CaptureTemplateEngineTest extends JUnit4(CaptureTemplateEngineSpec)

object CaptureTemplateEngineSpec extends Specification {
  "CaptureTemplateEngine formatters" should {
    "csv -> add comma and space" in {CaptureTemplateEngine.formatterCSV.format("test") must_== "test, "}
    "scsv -> add comma and space" in {CaptureTemplateEngine.formatterSemiCSV.format("test") must_== "test; "}
    "modifier -> add + in front of positive int" in {CaptureTemplateEngine.formatterModifier.format("10") must_== "+10"}
    "modifier -> preserve - in front of negative int" in {CaptureTemplateEngine.formatterModifier.format("-10") must_== "-10"}
    "modifier -> do nothing if not int" in {CaptureTemplateEngine.formatterModifier.format("hello") must_== "hello"}
  }

  "CaptureTemplateEngine" should {
    for (fmt <- List("csv", "scsv", "modifier")) {
      "have formatter " + fmt in {CaptureTemplateEngine.engine.hasFormatter(fmt) must beTrue}
    }
  }

  "CaptureTemplateEngine as a TemplateStore" should {
    "get correct file from classname" in {
      val bo = CaptureTemplateEngine.getObjectUpdateAwareLoader("monster")
      bo match {
        case fbo: FileUpdateAwareLoader[Template] => fbo.file must_== new File(new File(Configuration.dataDirectory,"template"),"monster.xtmpl")
        case _  => fail("Should be a file backed object")
      }
    }

    "provide static template error template if something goes wrong" in {
      val bo = CaptureTemplateEngine.getObjectUpdateAwareLoader("no-there")
      val to = bo.getCurrent()
      to.isDefined must beTrue
      to.get.render(null).toString().startsWith("<html><body>Failed to load template") must beTrue
    }
  }

}