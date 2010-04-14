/**
 * Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.infra.util

/**
 * RoundRobin are sequences that can be accessed by an offset from the current head
 */
trait RoundRobin[T] {
  protected var robin: Seq[T]
  protected var headIdx: Int = -1

  def setRobin(aHead: T, aSeq: Seq[T])

  def advance() {
    if (!robin.isEmpty) {
      headIdx += 1
      if (headIdx > robin.length) headIdx = 0
    }
  }

  def headOption: Option[T] = if (robin.isEmpty) None else Some(this(0))

  def advanceTo(newHead: T) {
    val idx = robin.indexOf(newHead)
    if (idx == -1) throw new NoSuchElementException(newHead + " is not part o Robin")
    else headIdx = idx
  }

  def apply(idx: Int): T = {
    if (idx < 0 || idx > robin.length) throw new IndexOutOfBoundsException()
    var midx = idx + headIdx
    if (midx >= robin.length) midx = midx - robin.length
    robin(midx)
  }
}

/**
 * Create an Array backed RoundRobin.
 * @param aHead The head of the array, null if not is set, must be part of array
 * @param aSeq A sequence of array elements.
 */
class ArrayRoundRobin[T](aHead: T, aSeq: Seq[T]) extends RoundRobin[T] {
  protected var robin: Seq[T] = new Array[T](0)

  setRobin(aHead, aSeq)

  def setRobin(aHead: T, aSeq: Seq[T]) {
    robin = aSeq.toArray
    if (aHead != null) advanceTo(aHead)
  }

}