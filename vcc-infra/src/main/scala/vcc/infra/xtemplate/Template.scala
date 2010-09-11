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

import scala.xml._

/**
 * Provides information for the rendering of the template.
 */
trait TemplateDataSource {
  /**
   * String value for a given key.
   */
  def templateVariable(key:String):Option[String]

  /**
   * Return a data source for a nested TemplateDataSource.
   */
  def templateGroup(key:String):List[TemplateDataSource]

  /**
   * Return a XML NodeSeq for a give name.
   */
  def templateInlineXML(key:String):NodeSeq
}

/**
 * Template hold the master node for a template, and allow the definition of macros.
 */
class Template(val prefix: String) {

  private var macros = Map.empty[String,NodeSeq]

  private var topNode:Node = null

  /**
   * Returns the existence or a given macro.  
   */
  def hasMacro(name:String) = macros.isDefinedAt(name)

  /**
   * Define or redefine a macro.
   */
  def defineMacro(name:String, tn: NodeSeq) {
    if(macros.isDefinedAt(name))
      throw new IllegalArgumentException("Redefinition of macro '"+ name + "' is not allowed")
    macros = macros + (name -> tn)
  }

  /**
   * Return a macro, will not check for the existance.
   */
  def getMacro(name:String):NodeSeq = macros(name)


  private [xtemplate] def setTopNode(node: Node) {
    if(!node.isInstanceOf[Elem])
      throw new IllegalTemplateDirectiveException("Template top level Node must be an Elem",node)
    topNode = node
  }

  /**
   * Generate the XML resultant of applying this template to the data source supplied.
   * @para ds The source of information to be expanded in the template.
   * @return A single node representing the entire template 
   */
  def render(ds: TemplateDataSource): Node = {
    val ns = TemplateNode.renderNode(ds, topNode)
    if(ns.length == 1) ns(0)
    else throw new AssertionError("Rendering of topNode must yield a single node.")
  }
}
