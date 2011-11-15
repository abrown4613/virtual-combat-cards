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
import collection.mutable.ListBuffer

/**
 * This XML Node represents an resolved TemplateDirective in the XML template.
 */
final class TemplateNode[T](override val prefix: String, val label: String, val directive: TemplateDirective[T], val arguments: T, val child: Seq[Node]) extends Node {
  def render(ds: TemplateDataSource) = directive.render(ds, this)


  override def toString(): String = "[" + super.toString + "]"
}

/**
 * Several utility functions for the Template Engine.
 */
final object TemplateNode {

  /**
   * Merge adjacent scala.xml.Text nodes.
   * @para ns NodeSeq to be processed
   * @return NodeSeq with all adjacent text merged
   */
  def mergeTexts(ns: NodeSeq): NodeSeq = {
    if (ns.length > 1) {
      var sb: StringBuffer = null
      val result = new ListBuffer[Node]()
      var rest = ns

      while (!rest.isEmpty) {
        rest.head match {
          case Text(t) =>
            if (sb == null) sb = new StringBuffer(t)
            else sb.append(t)
          case s =>
            if (sb != null) {
              result += Text(sb.toString)
              sb = null
            }
            result += s
        }
        rest = rest.tail
      }
      //add last
      if (sb != null) result += Text(sb.toString)
      result.toSeq
    } else ns
  }

  /**
   * Recursively render node by expanding TemplateNode to the rendered form, and rewriting Elem to same definition with
   * renderNode applied to their children.
   * @para ds Datasource to be unsided on the TemplateDirectives
   * @para node Node to be formatted (will recurse through Elements)
   * @retun A new reformatted node sequence.
   */
  def renderNode(ds: TemplateDataSource, node: Node): NodeSeq = {
    node match {
      case tn: TemplateNode[_] => tn.render(ds)
      case e: Elem =>
        new Elem(e.prefix, e.label, e.attributes, e.scope, TemplateNode.mergeTexts(e.child.flatMap(n => renderNode(ds, n))): _*)
      case t => t
    }
  }
}
