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

import scala.xml.{NodeSeq, Node, MetaData}

/**
 * Indicates that template parts within this Directive must be rendered only if the TemplateDataSource contains a field (param 'id',
 * group (param 'group') or styledtext (param 'styled').
 */
object IfDefinedDirective extends TemplateDirective[TemplateDataSource => Boolean]("ifdefined", false) {
  def render(ds: TemplateDataSource, node: TemplateNode[TemplateDataSource => Boolean]): NodeSeq = {
    if(node.arguments(ds)) renderChildren(ds, node.child)
    else Nil
  }

  protected def processArguments(node: Node, engine: TemplateEngine, template: Template, child: NodeSeq): (TemplateDataSource) => Boolean = {
    if (node.attributes.length != 1)
      throw new IllegalTemplateDirectiveException("Must specify exactly ONE of: group, id, styled", node)
    val attr = node.attributes.toSeq(0)
    attr.key match {
      case "id" => ((ds: TemplateDataSource) => ds.templateVariable(attr.value.text).isDefined)
      case "group" => ((ds: TemplateDataSource) => !ds.templateGroup(attr.value.text).isEmpty)
      case "inline" => ((ds: TemplateDataSource) => !ds.templateInlineXML(attr.value.text).isEmpty)
      case s => throw new IllegalTemplateDirectiveException("Attribute '" + s + "' is not valid for ifdefined", node)
    }
  }
}