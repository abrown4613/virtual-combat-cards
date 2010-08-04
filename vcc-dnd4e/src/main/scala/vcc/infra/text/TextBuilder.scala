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
package vcc.infra.text

/**
 * This is a class to accumulate Block elements.
 */
class TextBuilder {
  private var blks:List[Block] = Nil

  /**
   * Append a new block to the end of the text.
   */
  def append(block:Block) {
    if(block == null) throw new IllegalArgumentException("Must provide a block")
    blks = block :: blks
  }

  /**
   *  Return the current styled text (i.e. a list of blocks).
   */
  def getDocument:StyledText = StyledText(blks.reverse)
}