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

import xml.{Text, NodeSeq, Node}

/**
 * Print the content of a TemplateDataSource value if it is defined. Or nothing othewise.
 */
object EchoDataDirective extends TemplateDirective[(String, TemplateFormatter)]("data", true) {
  def render(ds: TemplateDataSource, node: TemplateNode[(String, TemplateFormatter)]): NodeSeq = {
    val v = ds.get(node.arguments._1)
    if (v.isDefined) Seq(Text(if (node.arguments._2 != null) node.arguments._2.format(v.get) else v.get))
    else Nil
  }

  protected def processArguments(node: Node, engine: TemplateEngine): (String, TemplateFormatter) = {
    val id = getAttribute(node, "id", null)
    if (id == null) throw new IllegalTemplateDirectiveException("must specify the data 'id'", node)
    val fmt = getAttribute(node, "fmt", null)
    val formatter = if (fmt == null) null
    else {
      if (!engine.hasFormatter(fmt))
        throw new IllegalTemplateDirectiveException("Parameter 'fmt' does not refer to defined formatter", node)
      engine.getFormatter(fmt)
    }
    (id, formatter)
  }
}