/**
 *   Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.domain.dndi

trait DNDIObject {

  /**
   *  Entry ID in the DNDI sites
   */
  val id: Int

  /**
   * Entry type or class in the DNDI site
   */
  val clazz: String

  /**
   * Attributes at the top level object. Note that keys are assumed to be upper cased.
   */
  protected var attributes: Map[String, String]

  /**
   * Gets the value of an attribute is defined.
   * @para attribute Name of the attribute
   * @return Option of the attribute value
   */
  def apply(attribute: String): Option[String] = {
    val normalizedAttr = attribute.toUpperCase
    if (attributes.contains(normalizedAttr)) Some(attributes(normalizedAttr))
    else None
  }

}