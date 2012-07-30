/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.advtools

import scala.xml.{Node, NodeSeq}

trait XmlReaderMixin {
  protected def emptyOrStringAsOption(name: String): Option[String] = if (name == "") None else Some(name)

  protected def getReferencedObjectName(node: Node): String = (node \ "ReferencedObject" \ "Name").text

  protected def formatOption(option: Option[String], fmt: String, default: String = ""): String =
    option.map(fmt.format(_)).getOrElse(default)

  protected def optionalValue(ns: NodeSeq): Option[String] = if (ns.isEmpty) None else emptyOrStringAsOption(ns(0).text)

}