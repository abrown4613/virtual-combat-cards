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

import scala.xml.{Node, NodeSeq}

/**
 *
 */
class IllegalTemplateDirectiveException(msg: String, node: Node) extends Exception(msg + "; offending node: " + node)

/**
 * TemplateDirective represents a macro or action that will be rendered to XML when processing. The implementation must
 * also provide way to validated the parameters of an template entry that will call the Directive.
 */
abstract class TemplateDirective[T](val name: String, val empty: Boolean) {

  /**
   * This method is used to validate and rearrange the arguments in the TemplateDirective. Implementation should validate
   * input node to make sure it can be rendered.
   * @para node The node that represents the template directive call in the template, should have all the arguments in it.
   * @para engine This is the engine that contains all define TemplateDirective and Formatter
   * @return Returns expected parameters for the future invocation of the template directive. null is a valid option.
   * @throws IllegalTemplateDirectiveException If the node not correctly formatted, this exception should be thrown.
   */
  @throws(classOf[IllegalTemplateDirectiveException])
  protected def processArguments(node: Node, engine: TemplateEngine, template: Template, child: NodeSeq): T

  final def resolveTemplateNode(node: Node, engine: TemplateEngine, template: Template, child: NodeSeq): TemplateNode[T] = {
    if (node.label == name) {
      if (empty && !node.child.isEmpty)
        throw new IllegalTemplateDirectiveException("Directive does does not allow child nodes", node)
      new TemplateNode(node.prefix, name, this, processArguments(node, engine, template, child), child)
    } else
      throw new AssertionError("node label must match Directive name")
  }

  /**
   * This method is called when processing a template to expand the TemplateNode in the template.
   * @para ds This TemplateDataSource that will be used to provide data to the rendering process.
   * @return A NodeSeq without any TemplateNode in it.
   */
  def render(ds: TemplateDataSource, node: TemplateNode[T]): NodeSeq

  /**
   * Helper function to extract an Attribute or a default value.
   * @para node To check for the attribute
   * @para name Attribute name
   * @para default Value to use if not found, can be null
   * @return Text of the attribute or default value if not found 
   */
  final protected def getAttribute(node: Node, name: String, default: String): String = {
    val a = node.attribute(name)
    if (a.isDefined) a.get(0).text
    else default
  }

  final protected def renderChildren(ds: TemplateDataSource, child: NodeSeq): NodeSeq = {
    TemplateNode.mergeTexts(child.flatMap(node => TemplateNode.renderNode(ds, node)))
  }

  /**
   * This is a help method that makes sure the Directive has only one argument and returns that argument value.
   * @param name Name of the argument to be searched for
   * @return The value of the arguments
   * @throws IllegalTemplateDirectiveException If argument not found, of if more than one argument passed on node.
   **/
  final protected def validateSingleArgument(name: String, node: Node): String = {
    val attr = getAttribute(node, name, null)
    if (attr != null) {
      if (node.attributes.length != 1)
        throw new IllegalTemplateDirectiveException("Only '" + name + "' argument accepted", node)
      attr
    } else throw new IllegalTemplateDirectiveException("Must specify '" + name + "' argument", node)
  }
}
