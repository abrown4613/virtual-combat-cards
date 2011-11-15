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
package vcc.dndi.reader

import java.io.PrintStream
import vcc.dndi.reader.Parser.BlockElement

/**
 * Common parsing logic for reading captured D&D Insider objects.
 */
trait DNDIObjectReader[T <: DNDIObject] {
  def process(blocks: List[BlockElement]): T

  def read(blocks: List[BlockElement]): DNDIObject = this.process(blocks)
}

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
   * Attributes at the top level object. Note that keys are assumed to be lowercase.
   */
  protected var attributes: Map[String, String]

  /**
   * Gets the value of an attribute is defined.
   * @para attribute Name of the attribute
   * @return Option of the attribute value
   */
  def apply(attribute: String): Option[String] = attributes.get(attribute.toLowerCase)

  def dump(os: PrintStream) {
    os.printf("----- Begin DNIDObject %s-%s\n", clazz, id.toString)
    for ((k, v) <- attributes) {
      os.printf("\t[%s] = '%s'\n", k, v)
    }
    os.printf("----- End DNIDObject %s-%s\n", clazz, id.toString)
  }
}
