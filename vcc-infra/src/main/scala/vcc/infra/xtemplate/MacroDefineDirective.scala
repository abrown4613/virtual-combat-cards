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

import scala.xml.{NodeSeq, Node}

/**
 * Defines a macro by adding its processed childs to the Template macros.
 */
object MacroDefineDirective extends TemplateDirective[String]("define", false) {
  def render(ds: TemplateDataSource, node: TemplateNode[String]): NodeSeq = Nil

  /**
   * Adds a symbol to the Template able
   */
  protected def processArguments(node: Node, engine: TemplateEngine, template: Template, child: NodeSeq): String = {
    val macro = validateSingleArgument("macro", node)
    if (template.hasMacro(macro))
      throw new IllegalTemplateDirectiveException("Macro '" + macro + "' is already defined.", node)
    template.defineMacro(macro, child)
    macro
  }
}

/**
 * Render a previously stored Macro (fetched from the template), in the position in was placed.
 */
object MacroIncludeDirective extends TemplateDirective[(String, NodeSeq)]("include", true) {
  def render(ds: TemplateDataSource, node: TemplateNode[(String, NodeSeq)]): NodeSeq = renderChildren(ds,node.arguments._2)

  protected def processArguments(node: Node, engine: TemplateEngine, template: Template, child: NodeSeq): (String, NodeSeq) = {
    val macro = validateSingleArgument("macro", node)
    if (!template.hasMacro(macro))
      throw new IllegalTemplateDirectiveException("Macro '" + macro + "' not defined", node)
    (macro, template.getMacro(macro))
  }
}