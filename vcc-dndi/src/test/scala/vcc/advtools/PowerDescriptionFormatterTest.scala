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

import org.specs2.mutable.SpecificationWithJUnit
import vcc.infra.text._

class PowerDescriptionFormatterTest extends SpecificationWithJUnit {

  private val tagClass = "flavorIndent"

  "format a Block text" in {
    val block = buildBlock(
      makeEntry(tagClass, "Attack: ", "Melee 3; +11 vs. AC"),
      makeEntry(tagClass, "Hit: ", "1d8+4 slowed EOT"))
    val r = FormattedTextParser.parseBlock("_Attack: _Melee 3; +11 vs. AC\n_Hit: _1d8+4 slowed EOT").get
    PowerDescriptionFormatter.formatAttack(r) must_== block
  }

  private def makeEntry(clazz: String, header: String, body: String) = TextBlock("P", clazz,
    TextSegment.makeItalic(header),
    TextSegment(body))

  private def buildBlock(blocks: TextBlock*) = {
    val bb = new TextBuilder
    for (b <- blocks) bb.append(b)
    bb.getDocument()
  }
}