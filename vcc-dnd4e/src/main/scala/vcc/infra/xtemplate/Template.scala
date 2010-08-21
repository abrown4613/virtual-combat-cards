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
import vcc.infra.text.StyledText

trait TemplateDataSource {
  def get(key:String):Option[String]
  def group(key:String):List[TemplateDataSource]
  def getStyledText(key:String):Option[StyledText]
}
class Template(val prefix: String) {
  def renderNode(ds: TemplateDataSource, node: Node):NodeSeq = {
    if (node.prefix == prefix) {
      node.label match {
        case "data" =>
          val v = ds.get(node.attribute("id").get(0).text)
          if (v.isDefined) Seq(Text(v.get))
          else Nil
      }
    } else {
      node match {
        case e: Elem =>
          new Elem(e.prefix, e.label, e.attributes, e.scope, TemplateUtil.mergeTexts(e.child.flatMap(n => renderNode(ds, n))): _*)
        case t => t
      }
    }
  }
}