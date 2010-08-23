/*
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
package vcc.domain.dndi

import java.awt.Dimension
import vcc.util.swing.{MigPanel, XHTMLPaneAgent, XHTMLPane}
import swing.{Button, Action, MainFrame}
import vcc.infra.xtemplate.{TemplateLoader, TemplateEngine}
import org.xml.sax.InputSource
import java.io.{FileInputStream, File}

object StatBlockGenerateAndView {
  val baseDir = new File("vcc-dnd4e/fs-wc")
  XHTMLPaneAgent.createInstance(baseDir)
  println("File exists: " + (baseDir.getAbsolutePath) + (if (baseDir.exists) " exists" else " not exists"))

  val xhtmlPane = new XHTMLPane()
  val reloadButton = new Button(Action("Regenerate template") {
    this.regenerateTemplate()
  })

  val mainWindow = new MainFrame {
    title = this.getClass.getSimpleName
    contents = new MigPanel("fill", "[350]", "[40][500]") {
      add(reloadButton, "wrap")
      add(xhtmlPane, "growx, growy")
    }
    minimumSize = new Dimension(500, 600)
  }

  var monster: MonsterNew = null
  var templateFile: File = null

  private val loader = CaptureTemplateEngine.getLoader()

  def regenerateTemplate() {
    monster.dump(System.out)
    try {
      val template = loader.load(new InputSource(new FileInputStream(templateFile)))
      val xml = template.render(monster)
      xhtmlPane.setDocumentFromText(xml.toString)

    } catch {
      case e =>
        e.printStackTrace
        xhtmlPane.setDocumentFromText("<html><body>" + e.getMessage() + "</body></html>")

    }
  }

  def main(args: Array[String]) {
    org.apache.log4j.BasicConfigurator.configure();
    if (args.length != 2) {
      System.err.println("Please specify a a monster file and a template file.")
      exit
    }

    val file = new File(args(0))
    if (!file.exists) {
      System.err.println("Must give the name of an existant file, you specified: " + file.getAbsolutePath)
      exit()
    }

    templateFile = new File(args(1))
    if (!templateFile.exists) {
      System.err.println("Must give the name of an existant file, you specified: " + templateFile.getAbsolutePath)
      exit()
    }

    monster = try {
      val xml = scala.xml.XML.loadFile(file)
      val blocks = Parser.parseBlockElements(xml.child, true)
      if (blocks != null && !blocks.isEmpty) {
        val mReader = new MonsterReader(0)
        val monster = mReader.process(blocks)
        monster
      } else null
    } catch {
      case e =>
        System.err.println("Failed to parse: " + file + " reason: " + e.getMessage)
        null
    }
    if (monster != null) {
      mainWindow.visible = true
    } else {
      System.err.println("Failed to load monster..")
      exit
    }
  }
}