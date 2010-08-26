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
//$Id$
package vcc.domain.dndi

import org.xml.sax.InputSource
import vcc.dnd4e.Configuration
import vcc.infra.xtemplate.{Template, FunctionTemplateFormatter, TemplateLoader, TemplateEngine}
import org.slf4j.LoggerFactory
import java.io.{StringReader, FileInputStream, File}
import vcc.infra.diskcache.{UpdateableObjectStore, FileUpdateAwareLoader, UpdateAwareLoader, UpdateableObjectStoreResolver}

object CaptureTemplateEngine extends UpdateableObjectStoreResolver[String, Template] {
  private[dndi] val engine = new TemplateEngine()
  private val loader = new TemplateLoader("t", engine)
  private val logger = LoggerFactory.getLogger("domain")

  val formatterCSV = new FunctionTemplateFormatter("csv", s => s.formatted("%s, "))
  val formatterSemiCSV = new FunctionTemplateFormatter("scsv", s => s.formatted("%s; "))
  val formatterModifier = new FunctionTemplateFormatter("modifier", s => try {Integer.parseInt(s).formatted("%+d")} catch {case _ => s})

  engine.registerDefaultDirectives()
  engine.registerFormatter(formatterCSV)
  engine.registerFormatter(formatterSemiCSV)
  engine.registerFormatter(formatterModifier)

  /**
   * Get TemplateLoader
   */
  def getObjectUpdateAwareLoader(clazz: String): UpdateAwareLoader[Template] = {
    val file = new File(templateDirectory, clazz + ".xtmpl")
    new FileUpdateAwareLoader(file, loadTemplateFromFile(_))
  }

  private[dndi] def loadTemplateFromFile(file : File): Option[Template] = {
    val t:Template = try {
      loader.load(new InputSource(new FileInputStream(file)))
    } catch {
      case e =>
        logger.error("Failed to load template from file = {}",Array(file.getAbsolutePath),e)
        val reader = new StringReader("<html><body>Failed to load template "+file.getAbsolutePath +"; reason: "+e.getMessage+"</body></html>")
        loader.load(new InputSource(reader))
    }
    if(t != null) Some(t) else None
  }

  private val templateDirectory: File = new File(Configuration.dataDirectory, "template")

  private val store = new UpdateableObjectStore[String,Template](this)

  def fetchClassTemplate(clazz: String) = store.fetch(clazz).get
}