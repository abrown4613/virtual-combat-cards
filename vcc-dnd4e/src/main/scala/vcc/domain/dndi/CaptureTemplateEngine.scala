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

import vcc.infra.xtemplate.{FunctionTemplateFormatter, TemplateLoader, TemplateEngine}

object CaptureTemplateEngine {
  private[dndi] val engine = new TemplateEngine()
  private val loader = new TemplateLoader("t", engine)

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
  def getLoader() = loader

}