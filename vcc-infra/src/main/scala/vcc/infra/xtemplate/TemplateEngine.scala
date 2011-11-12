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
package vcc.infra.xtemplate

trait TemplateFormatter {
  val name: String

  def format(raw: String): String
}

case class FunctionTemplateFormatter(val name:String, fmt: String=>String) extends TemplateFormatter {
  if(fmt == null) throw new IllegalArgumentException("Must specify format function.")
  def format(raw: String): String = fmt(raw)
}

/**
 * This class is essencially a symbol table to hold TemplateFormatter and TemplateDirective for the loader resolution.
 */
class TemplateEngine {
  private val formatter = scala.collection.mutable.Map.empty[String, TemplateFormatter]
  private val directive = scala.collection.mutable.Map.empty[String, TemplateDirective[_]]

  def hasFormatter(name: String) = formatter.isDefinedAt(name)

  def getFormatter(name: String): TemplateFormatter = formatter(name)

  def registerFormatter(fmt: TemplateFormatter) {
    if (formatter.isDefinedAt(fmt.name)) throw new IllegalArgumentException("Cannot register " + fmt.name + " twice.")
    formatter += (fmt.name -> fmt)
  }

  def hasDirective(name: String) = directive.isDefinedAt(name)

  def getDirective(name: String): TemplateDirective[_] = directive(name)

  def registerDirective(td: TemplateDirective[_]) {
    if (directive.isDefinedAt(td.name)) throw new IllegalArgumentException("Cannot register " + td.name + " twice.")
    directive += (td.name -> td)
  }

  /**
   * Loads all xtemplate defined directive to the engine.
   */
  def registerDefaultDirectives() {
    this.registerDirective(EchoDataDirective)
    this.registerDirective(IfDefinedDirective)
    this.registerDirective(GroupDirective)
    this.registerDirective(InlineXMLDirective)
    this.registerDirective(MacroDefineDirective)
    this.registerDirective(MacroIncludeDirective)
  }
}