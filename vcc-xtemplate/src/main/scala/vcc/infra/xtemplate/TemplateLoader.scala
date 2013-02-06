/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.infra.xtemplate

import scala.xml._

/**
 * Loads Templates and verifies proper binding to the engine provided.
 * @param defaultPrefix The prefix to look for in the XML Elements, any one with this prefix will be checked against the
 * engine defined TemplateDirective
 * @param engine Engine containing the definition of TemplateDirective and TemplateFormatter
 */
class TemplateLoader(defaultPrefix: String, engine: TemplateEngine) {

  /**
   * Resolves a template node, by either copying it with it resolved children, or expanding TemplateDirective.
   */
  private[xtemplate] def resolveNode(node: Node, template: Template): Node = {
    //First resolver child
    val rChild = node.child.map(c => resolveNode(c, template))
    if (node.prefix == defaultPrefix) {
      if (engine.hasDirective(node.label)) {
        val directive = engine.getDirective(node.label)
        directive.resolveTemplateNode(node, engine, template, rChild)
      } else {
        throw new IllegalTemplateDirectiveException("Directive '" + node.label + "' not defined", node)
      }
    } else {
      node match {
        case e: Elem => new Elem(e.prefix, e.label, e.attributes, e.scope, true, rChild: _*)
        case t => t
      }
    }
  }

  /**
   * Loads an XML file from an InputSource then processes all the nodes to provided a resolved Template.
   * @param is The InputSource for the XML data
   * @return A Resolved template.
   * @throws IllegalTemplateDirectiveException if template expansion produced an error
   */
  @throws(classOf[IllegalTemplateDirectiveException])
  def load(is: InputSource):Template = {
    if(is == null) throw new IllegalArgumentException("InputSource must not be null")
    val xml = XML.load(is)
    val template = new Template(defaultPrefix)
    template.setTopNode(resolveNode(xml, template))
    template
  }
}