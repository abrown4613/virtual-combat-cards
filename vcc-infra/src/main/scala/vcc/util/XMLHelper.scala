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
package vcc.util

import scala.xml.NodeSeq

object XMLHelper {

  def nodeSeq2String(ns: NodeSeq): String = {
    if (ns != null && ns.length == 1) ns.text
    else throw new Exception("Invalid NodeSeq, should contains a single node")
  }

  def nodeSeq2String(ns: NodeSeq, default: String): String = {
    if (ns != null && ns.length == 1) ns.text
    else default
  }

  def nodeSeq2Int(ns: NodeSeq) = nodeSeq2String(ns).toInt

  def nodeSeq2Int(ns: NodeSeq, default: Int) = try {
    nodeSeq2String(ns).toInt
  } catch {
    case _: Exception => default
  }

}