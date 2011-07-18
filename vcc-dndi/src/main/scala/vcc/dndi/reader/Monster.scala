/**
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

package vcc.dndi.reader

import scala.xml.{NodeSeq}
import vcc.infra.xtemplate.TemplateDataSource

/**
 * A monster imported from DNDI.
 * @param id DNDI ID value
 * @param legacyPowers list of power that where not in their own power sections
 * @param powersByAction Powers that are in sections, this will include Auras moved from previous format
 */
class Monster(val id: Int,
                 protected var attributes: Map[String, String],
                 val legacyPowers: List[Power],
                 val powersByAction: Map[ActionType.Value, List[Power]]
        ) extends DNDIObject with TemplateDataSource {
  final val clazz = "monster"

  def templateGroup(key: String): List[TemplateDataSource] = key match {
    case "legacy" => legacyPowers
    case ActionType(at) => powersByAction.getOrElse(at,Nil)
    case _ => Nil
  }

  // No inlines yet
  def templateInlineXML(key: String): NodeSeq = Nil


  def templateVariable(key: String): Option[String] = this(key)
}