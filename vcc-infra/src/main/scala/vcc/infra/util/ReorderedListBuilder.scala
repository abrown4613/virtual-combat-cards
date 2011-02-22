/**
 * Copyright (C) 2008-2011 - Thomas Santana <tms@exnebula.org>
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
 * Provides a way to determine if two elements are larger than each other.
 * Since ordering object may involve arbitrarily complex calculations this trait allows elements to include
 * complex processes that may not be limited to the objects being compared.
 */
trait ReorderedListBuilderCompare[T] {
  /**
   * Return is a should be placed before b for the purpose of sorting entries
   */
  def isBefore(a: T, b: T): Boolean
}

/**
 * An object that can be uniquely identified. This provide a mean to see the UniqueIdentifier of the object.
 */
trait UniquelyIdentified[K] {
  def uniqueId: K
}

class DuplicateElementException(msg: String) extends Exception(msg)

class NotInOrderException(msg: String) extends Exception(msg)

/**
 *  This is a generic class to build initiative orders which may include reorder. This
 * is needed for 4th edition and 3.5, hence a generic approach.
 * You need to provide the a base list (can be Nil) then add entries. These will be sorted using
 * a tieBreaker function. A reorder is a Pair[T,T] of (a,b) with the following meaning a will be placed
 * before b.
 * The list does not accept repeated object.
 * @param initList A list of T which must be ordered and not contain any duplicated
 * @param initReorder A list of reorder operation, elements must be part of baseList
 * @param comparator An object that can compara two elements of T
 */
class ReorderedListBuilder[K, T <: UniquelyIdentified[K]](initList: List[T], initReorder: List[(K, K)], comparator: ReorderedListBuilderCompare[T]) {

  /**
   * These elements have two order, base list as defined by comparator and a reorder next and prev pointing to the
   * previous element in that list.
   * @param e The element being added
   * @param next The next in base line order
   * @param prevReorder The previous on the reorder
   * @param nextReorder The next on the reoder list
   */
  private class ListLink(val elem: T, var next: ListLink, var prevReorder: ListLink, var nextReorder: ListLink) {
    def findLink(search: K): ListLink = {
      var p = this
      while (p != null) {
        if (p.elem.uniqueId == search) return p
        p = p.next
      }
      null
    }

    def contains(search: K) = findLink(search) != null
  }

  private class DirectListIterator(private var p: ListLink) extends Iterator[T] {
    def next = {
      val r = p.elem
      p = p.next
      r
    }

    def hasNext = p != null
  }


  private var _baseList: ListLink = null
  private var _last: ListLink = null
  private var _reorder: List[(K, K)] = Nil
  sanitize()

  //Build list and sanitize input parameters and build list
  private def sanitize() {
    var p: ListLink = null
    for (e <- initList) {
      val ne = new ListLink(e, null, null, null)
      if (_baseList == null) _baseList = ne
      else if (_baseList.contains(e.uniqueId)) throw new DuplicateElementException("An entry of " + e + "already exists in the list")
      if (p != null) {
        if (!comparator.isBefore(p.elem, e)) throw new NotInOrderException("Entry " + p.elem + " is not less than it " + e)
        p.next = ne
      }
      else _baseList = ne
      p = ne
    }

    for (p <- initReorder) {
      if (p._1 == p._2) throw new IllegalArgumentException("Entries of reorder list must contain different elements, found: " + p)
      if (!_baseList.contains(p._1)) throw new NoSuchElementException("Element" + p._1 + " not found in base list")
      if (!_baseList.contains(p._2)) throw new NoSuchElementException("Element" + p._2 + " not found in base list")
    }
    _reorder = initReorder
  }


  /**
   * Add a new entry in is correct place of the ordered list
   */
  def addEntry(e: T) {
    if (_baseList != null) {
      if (_baseList.contains(e.uniqueId)) throw new DuplicateElementException("An entry of " + e + "already exists in the list")
      //Insert into position
      val ne = new ListLink(e, null, null, null)
      var p = _baseList
      var last: ListLink = null
      while (p != null && comparator.isBefore(p.elem, e)) {
        last = p
        p = p.next
      }
      if (last == null) {
        ne.next = _baseList
        _baseList = ne
      } else {
        ne.next = last.next
        last.next = ne

      }
    } else {
      _baseList = new ListLink(e, null, null, null)
    }
    _last = null //Force reorder
  }

  /**
   * Add a new reorder command to the list.
   * Elements must be part of the base list.
   * @param elem The element to be moved
   * @param before Element to be move before
   */
  def addReorder(elem: K, before: K) {
    if (elem == before) throw new IllegalArgumentException(elem + " and " + before + " must be different")
    if (_baseList != null && _baseList.contains(elem) && _baseList.contains(before)) {
      _reorder = _reorder ::: List((elem, before))
    } else throw new NoSuchElementException("Base list is does not contain " + elem + " or " + before)
  }

  /**
   *  Get the base list with the reorders applied.
   * @return A reordered list of T
   */
  def reorderedList(): List[K] = {
    var p = _baseList
    if (_last == null) {
      var last: ListLink = null

      // Build reverse list
      while (p != null) {
        p.prevReorder = last
        p.nextReorder = p.next
        last = p
        p = p.next
      }
      _last = last

      for ((elem, before) <- _reorder) {
        val elemLink = _baseList.findLink(elem)
        val befLink = _baseList.findLink(before)
        //Take element out of d-link
        if (elemLink.nextReorder == null) {
          _last = elemLink.prevReorder
          elemLink.prevReorder.nextReorder = null
        } else if (elemLink.prevReorder == null) {
          elemLink.nextReorder.prevReorder = null
        } else {
          elemLink.prevReorder.nextReorder = elemLink.nextReorder
          elemLink.nextReorder.prevReorder = elemLink.prevReorder
        }
        //           Prev to B PTB
        //            Before:            After
        //            PTB ---> Before  PTB ---> Elem ---> Before
        //            PTB <--- Before  PTB <--- Elem <--- Before
        elemLink.prevReorder = befLink.prevReorder
        elemLink.nextReorder = befLink
        if (elemLink.prevReorder != null) elemLink.prevReorder.nextReorder = elemLink
        befLink.prevReorder = elemLink
      }

    }

    //Build return using reverse order
    p = _last
    var ret: List[K] = Nil
    while (p != null) {
      ret = p.elem.uniqueId :: ret
      p = p.prevReorder
    }
    ret
  }

  /**
   * Get list of reorder commands.
   */
  def reorders(): List[(K, K)] = _reorder

  /**
   * Get the baseList after the additions.
   */
  def baseList(): List[T] = (new DirectListIterator(_baseList)).toList
}