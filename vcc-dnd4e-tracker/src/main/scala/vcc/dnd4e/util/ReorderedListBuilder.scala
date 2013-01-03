/*
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
package vcc.dnd4e.util

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
 * a tieBreaker function. A reorder is a Pair[K,K] of (a,b) with the following meaning a will be placed
 * before b.
 * The list does not accept repeated object.
 * @param initList A list of T which must be ordered and not contain any duplicated
 * @param initReorder A list of reorder operation, elements must be part of baseList
 * @param comparator An object that can compara two elements of T
 */
class ReorderedListBuilder[K, T <: UniquelyIdentified[K]](initList: List[T], initReorder: List[(K, K)], comparator: ReorderedListBuilderCompare[T]) {

  private var baseOrder: List[T] = Nil
  private var reorderingList: List[(K, K)] = Nil

  verifyCorrectnessOfBuildParameters()

  /**
   * Add a new entry in is correct place of the ordered list
   */
  def addEntry(entry: T) {
    checkElementNotInBaseList(entry)
    baseOrder = insertElementAccordingToOrder(baseOrder)

    def insertElementAccordingToOrder(list: List[T]): List[T] = {
      val (itemsBeforeInsertPoint, itemsAfterInsertPoint) = list.splitAt(insertPointOrEndOfList(list))
      itemsBeforeInsertPoint ::: List(entry) ::: itemsAfterInsertPoint
    }

    def insertPointOrEndOfList(list: List[T]): Int = {
      val index = list.indexWhere(e => comparator.isBefore(entry, e))
      if (index == -1) list.length else index
    }
  }

  /**
   * Add a new reorder command to the list.
   * Elements must be part of the base list.
   * @param elem The element to be moved
   * @param before Element to be move before
   */
  def addReorder(elem: K, before: K) {
    throwExceptionOnIdenticalKeysInReorderList(elem,before)
    throwExceptionIfKeyNotInOrder(elem)
    throwExceptionIfKeyNotInOrder(before)

    reorderingList = reorderingList ::: List((elem, before))
  }

  /**
   *  Get the base list with the reorders applied.
   * @return A reordered list of T
   */
  def reorderedList(): List[K] = {

    def matchKey(toMove: K): (T) => Boolean = (_.uniqueId == toMove)

    def moveBefore(order: List[T], toMove: K, before: K): List[T] = {
      val listWithoutToMove: List[T] = order.filterNot(matchKey(toMove))
      val (bef, after) = listWithoutToMove.splitAt(listWithoutToMove.indexWhere(matchKey(before)))
      bef ::: order.find(matchKey(toMove)).toList ::: after
    }

    var initialList = baseOrder
    for ((elem, before) <- reorderingList) {
      initialList = moveBefore(initialList, elem, before)
    }
    initialList.map(_.uniqueId)
  }

  /**
   * Get list of reorder commands.
   */
  def reorders(): List[(K, K)] = reorderingList

  /**
   * Get the baseList after the additions.
   */
  def baseList(): List[T] = baseOrder

  private def throwExceptionOnIdenticalKeysInReorderList(a: K, b: K) {
    if (a == b)
      throw new IllegalArgumentException("Entries of reorder list must contain different elements, found: " +(a, b))
  }

  private def verifyCorrectnessOfBuildParameters() {
    mustNotHaveDuplicateKey(initList)
    checkOrderingOfElements(initList)
    baseOrder = initList

    verifyAllEntriesInReorderAreValid()
    reorderingList = initReorder

    def mustNotHaveDuplicateKey(list: List[T]) {
      var alreadyDefined = Set.empty[K]
      for (element <- list) {
        if (alreadyDefined.contains(element.uniqueId))
          throw new DuplicateElementException("Base order already contains one : " + element.uniqueId)
        alreadyDefined = alreadyDefined + element.uniqueId
      }
    }

    def checkOrderingOfElements(list: List[T]) {
      for ((a, b) <- inPairs(list)) {
        mustBeInOrder(a, b)
      }

      def inPairs[A](list: List[A]): List[(A, A)] = {
        if (list.length > 1)
          (list.head, list(1)) :: inPairs(list.tail)
        else
          Nil
      }

      def mustBeInOrder(a: T, b: T) {
        if (!comparator.isBefore(a, b))
          throw new NotInOrderException("Entry " + a + " is not less than it " + b)
      }
    }

    def verifyAllEntriesInReorderAreValid() {
      for ((a, b) <- initReorder) {
        throwExceptionOnIdenticalKeysInReorderList(a, b)
        throwExceptionIfKeyNotInOrder(a)
        throwExceptionIfKeyNotInOrder(b)
      }
    }
  }

  private def checkElementNotInBaseList(element: T) {
    if (keyExistsInBaseOrder(element.uniqueId))
      throw new DuplicateElementException("An entry of " + element + "already exists in the list")
  }

  private def throwExceptionIfKeyNotInOrder(key: K) {
    if (!keyExistsInBaseOrder(key))
      throw new NoSuchElementException("Element" + key + " not found in base list")
  }

  private def keyExistsInBaseOrder(key: K): Boolean = {
    baseOrder.exists(_.uniqueId == key)
  }
}