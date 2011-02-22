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

import org.specs.SpecificationWithJUnit

class ReorderListBuilderTest extends SpecificationWithJUnit {
  var theList: ReorderedListBuilder[Int, UIInt] = null

  object comparator extends ReorderedListBuilderCompare[UIInt] {
    def isBefore(a: UIInt, b: UIInt) = (a.uniqueId - b.uniqueId) < 0
  }

  case class UIInt(override val uniqueId: Int) extends UniquelyIdentified[Int]

  implicit def int2UIInt(i: Int): UIInt = new UIInt(i)

  implicit def listInt2ListUIInt(lst: List[Int]): List[UIInt] = lst.map(int2UIInt(_))


  val emptyList = beforeContext {
    theList = new ReorderedListBuilder[Int, UIInt](Nil, Nil, comparator)
  }

  "ReorderListBuilder constructor" should {
    "reject duplicated elements on base list" in {
      new ReorderedListBuilder[Int, UIInt](List(1, 2, 10, 10), Nil, comparator) must throwA[DuplicateElementException]
    }

    "reject unsorted base list" in {
      new ReorderedListBuilder[Int, UIInt](List(1, 3, 10, 4, 5), Nil, comparator) must throwA[NotInOrderException]
    }

    "reject reorder list with non existant before" in {
      new ReorderedListBuilder[Int, UIInt](List(1, 2, 3, 4, 5), List((10, 1)), comparator) must throwA[NoSuchElementException]
    }

    "reject reorder list with non existant after" in {
      new ReorderedListBuilder[Int, UIInt](List(1, 2, 3, 4, 5), List((1, 10)), comparator) must throwA[NoSuchElementException]
    }

    "reject reorder list with repeated elements" in {
      new ReorderedListBuilder[Int, UIInt](List(1, 2, 3, 4, 5), List((1, 1)), comparator) must throwA[IllegalArgumentException]
    }

    "accept a valid builder preserving the data" in {
      val theList = new ReorderedListBuilder[Int, UIInt](List(1, 2, 3, 4, 5), List((3, 1), (4, 2)), comparator)
      theList.reorders must_== List((3, 1), (4, 2))
      theList.baseList must_== List[UniquelyIdentified[Int]](1, 2, 3, 4, 5)
    }
  }

  "empty ReorderListBuilder" ->- (emptyList) should {

    "initialize correctly" in {
      theList.baseList must_== Nil
      theList.reorders must_== Nil
      theList.reorderedList must_== Nil
    }

    "add element" in {
      theList.addEntry(6)
      theList.baseList must_== List[UniquelyIdentified[Int]](6)
      theList.reorders must_== Nil
    }

    "reject duplication on add" in {
      theList.addEntry(6)
      theList.addEntry(6) must throwA[DuplicateElementException]
    }

    "correctly sort two elements placed in the wrong order" in {
      theList.addEntry(4)
      theList.addEntry(7)
      theList.addEntry(1)
      theList.addEntry(3)

      theList.baseList must_== List[UniquelyIdentified[Int]](1, 3, 4, 7)
    }

    "accept reoder of elements just added" in {
      theList.addEntry(4)
      theList.addEntry(7)
      theList.addEntry(1)

      theList.addReorder(1, 4)

      theList.reorders must_== List((1, 4))
    }

  }

  val baseList = beforeContext {
    theList = new ReorderedListBuilder[Int, UIInt](List(1, 3, 5, 7, 9), List((5, 1), (7, 3)), comparator)
  }

  "small ReorderListBuilder" ->- (baseList) should {

    "return the proper reorderd list" in {
      theList.reorderedList must_== List(5, 1, 7, 3, 9)
    }

    "add new element in proper place of base list" in {
      theList.addEntry(6)
      theList.baseList must_== List[UniquelyIdentified[Int]](1, 3, 5, 6, 7, 9)
    }

    "regenerate reorder list once we add an entity" in {
      theList.reorderedList must_== List(5, 1, 7, 3, 9)
      theList.addEntry(6)

      theList.reorderedList must_== List(5, 1, 7, 3, 6, 9)
    }

    "generate reorder list once we add an entity to the end" in {
      theList.reorderedList must_== List(5, 1, 7, 3, 9)
      theList.addEntry(12)
      theList.addReorder(12, 7)
      theList.reorderedList must_== List(5, 1, 12, 7, 3, 9)
    }
    "add element before all" in {
      theList.addEntry(0)
      theList.reorderedList must_== List(0, 5, 1, 7, 3, 9)
    }

    "add element before all and move it" in {
      theList.addEntry(0)
      theList.addReorder(0, 7)
      theList.reorderedList must_== List(5, 1, 0, 7, 3, 9)
    }
    "add a reorder to the list" in {
      theList.addReorder(3, 7)

      theList.reorders must_== List((5, 1), (7, 3), (3, 7))
    }

    "preserve base list when adding reorder" in {
      theList.addReorder(3, 7)

      theList.baseList must_== List[UniquelyIdentified[Int]](1, 3, 5, 7, 9)
    }

    "correctly apply new reorder" in {
      theList.addReorder(3, 7)

      theList.reorderedList must_== List(5, 1, 3, 7, 9)
    }

    "reject reorder of before not in list " in {
      theList.addReorder(5, 4) must throwA[NoSuchElementException]
    }

    "reject reorder of after not in list " in {
      theList.addReorder(4, 5) must throwA[NoSuchElementException]
    }

    "reject reorder of with same element" in {
      theList.addReorder(5, 5) must throwA[IllegalArgumentException]
    }
  }
}