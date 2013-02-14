/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.compendium

import org.specs2.SpecificationWithJUnit
import java.io.File
import vcc.infra.xtemplate.Template
import vcc.infra.diskcache.FileUpdateAwareLoader

class CaptureTemplateEngineTest extends SpecificationWithJUnit {

  private val dataDirectory: File = new File("../vcc-web/src/main/webapp")
  CaptureTemplateEngine.initialize(dataDirectory)

  def is = e1 ^ e2 ^ e3 ^ end

  def e1 = {
    "CaptureTemplateEngine formatters" ^
      "csv -> add comma and space" ! {
        CaptureTemplateEngine.formatterCSV.format("test") must_== "test, "
      } ^
      "scsv -> add comma and space" ! {
        CaptureTemplateEngine.formatterSemiCSV.format("test") must_== "test; "
      } ^
      "modifier -> add + in front of positive int" ! {
        CaptureTemplateEngine.formatterModifier.format("10") must_== "+10"
      } ^
      "modifier -> preserve - in front of negative int" ! {
        CaptureTemplateEngine.formatterModifier.format("-10") must_== "-10"
      } ^
      "modifier -> do nothing if not int" ! {
        CaptureTemplateEngine.formatterModifier.format("hello") must_== "hello"
      } ^
      endp
  }

  def e2 = {
    "CaptureTemplateEngine" ^
      (for (fmt <- List("csv", "scsv", "modifier")) yield {
        "have formatter " + fmt ! {
          CaptureTemplateEngine.getInstance.engine.hasFormatter(fmt) must beTrue
        }
      }) ^ endp
  }

  def e3 = {
    "CaptureTemplateEngine as a TemplateStore" ^
      "get correct file from classname" ! {
        val bo = CaptureTemplateEngine.getInstance.getObjectUpdateAwareLoader("monster")
        bo match {
          case fbo: FileUpdateAwareLoader[Template] =>
            fbo.file.getAbsolutePath must_== (new File(dataDirectory, "template/monster.xtmpl")).getAbsolutePath
          case _ => "" must_== "Should be a file backed object"
        }
      } ^
      "provide static template error template if something goes wrong" ! {
        val bo = CaptureTemplateEngine.getInstance.getObjectUpdateAwareLoader("no-there")
        val to = bo.getCurrent()
        to.isDefined must beTrue
        to.get.render(null).toString().startsWith("<html><body>Failed to load template") must beTrue
      } ^
      endp
  }
}