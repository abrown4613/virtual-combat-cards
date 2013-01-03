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

import vcc.infra.text.{TextSegment, TextBlock, TextBuilder, StyledText}
import vcc.dndi.common.FormattedText._

object PowerDescriptionFormatter {
  private var debug = false

  def doDebug() {
    debug = true
  }

  def formatAttack(block: Block): StyledText = {
    val builder = new TextBuilder
    for (line <- block.lines) {
      builder.append(TextBlock("P", getIndentClass(line.indent),
        line.parts.map(_ match {
          case Italic(t) => TextSegment.makeItalic(t)
          case Normal(t) => TextSegment(t)
        }): _*))
    }
    if (debug)
      builder.append(TextBlock("P", "", TextSegment.makeBold("RAW"), TextSegment(block.toString)))
    builder.getDocument()
  }

  private def getIndentClass(indent: Int): String = {
    indent match {
      case 0 => "flavorIndent"
      case 1 => "flavorIndent1"
      case _ => "flavorIndent2"
    }
  }
}