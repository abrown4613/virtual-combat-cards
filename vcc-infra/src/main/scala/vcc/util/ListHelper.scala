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
//$Id$
package vcc.util

object ListHelper {
  /**
   * Split the list at idx position and insert segment into the list
   * @param lst List to splice
   * @param idx Position to insert
   * @param segment Segment to be spliced
   * @return The list with the segment spliced in it. 
   */
  def splice[T](lst: List[T], idx: Int, segment: List[T]): List[T] = {
    if (idx < 0 || idx >= lst.length) throw new Exception("Out of bounds")
    lst.take(idx) ::: segment ::: lst.takeRight(lst.length - idx - 1)
  }

}