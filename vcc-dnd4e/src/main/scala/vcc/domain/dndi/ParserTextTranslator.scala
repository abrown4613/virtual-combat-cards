/**
 *  Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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

import vcc.infra.text._
import vcc.domain.dndi.Parser._

/**
 * Service object that provides a means of translating Parser parts to Styled text
 */
object ParserTextTranslator {

  /**
   * Transform a sequence of Parser part to their StyledText equivalent.
   * @para parts Parts to translate
   * @return Sequence of segments that is equivalent to the parser parts.
   */
  def partsToStyledText(parts: Seq[Parser.Part]): Seq[Segment] = {
    parts.map(part => part match {
      case Key(key) => TextSegment.makeBold(key)
      case Emphasis(em) => TextSegment.makeItalic(em)
      case Text(text) => TextSegment(text)
      case Break() => LineBreak
      case Icon(img) => InlineImage(Parser.IconType.iconToImage(img))
      case s => throw new Exception("Not styted text equivalent for: " + s)
    })
  }
}

