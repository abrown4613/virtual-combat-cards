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

import xml.NodeSeq

/**
 * This is a simple implementation of a TemplateDataSource backed by a series of maps. This can be used to generate quick
 * and simple templates data in maps.
 * @param values Are the values that will be provided by the templateVariable method.
 * @param groups This is a group of data sources that will be provided by the templateGroup method.
 * @param inline Represents a map of XML NodeSeq to be provided via templateInlineXML method.
 */
class MapDataSource(values: Map[String, String], groups: Map[String, List[TemplateDataSource]], inline: Map[String, NodeSeq])
        extends TemplateDataSource {
  def templateGroup(key: String): List[TemplateDataSource] = groups.getOrElse(key, Nil)

  def templateInlineXML(key: String): NodeSeq = inline.getOrElse(key, Nil)

  /**
   *
   */
  def templateVariable(key: String): Option[String] = {
    val ret = values.get(key)
    if (ret == Some(null)) None
    else ret
  }
}
