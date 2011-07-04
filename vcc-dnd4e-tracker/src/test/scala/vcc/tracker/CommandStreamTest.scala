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
//$Id$
package vcc.tracker

import org.specs2.SpecificationWithJUnit
import org.specs2.mock.Mockito

class CommandStreamTest extends SpecificationWithJUnit {
  def randomInt(): Int = scala.util.Random.nextInt()

  def streamDrainerIterator[A, S](stream: CommandStream[A, S], states: Seq[S]): Iterator[A] = new Iterator[A] {
    private var s = states
    private var cs = stream

    def next(): A = {
      val (r, ncs) = cs.next(s.head)
      this.cs = ncs
      s = s.tail
      r
    }

    def hasNext: Boolean = (!s.isEmpty && cs.hasNext(s.head))
  }

  def is =
    "SeqCommandStream" ^
      "  have no next on empty seq" ! scs1 ^
      "  have next if list has content" ! scs2 ^
      "  return first and continuation stream" ! scs3 ^
      "  return all on any sequence" ! scs4 ^
      "  throw exception on invalid next" ! scs4 ^
      endp ^
      "ChainedCommandStream" ^
      "  use first stream if it has something to provide" ! chainedStream().e1 ^
      "  use second stream if first does not supply values" ! chainedStream().e2 ^
      "  not have next if both are null" ! chainedStream().e3 ^
      "  throw exception on invalid next" ! chainedStream().e4 ^
      endp ^
      "PartialFunctionCommandStream" ^
      "  has next if PF isDefinedAt" ! partialStream().e1 ^
      "  not has next if PF isDefinedAt not true " ! partialStream().e2 ^
      "  return next if defined as PF.apply" ! partialStream().e3 ^
      "  throw exception on next with no hasNext" ! partialStream().e4 ^
      endp ^
      "all together" ! all1 ^
      end

  def scs1 = SeqCommandStream[Int, Int](Nil).hasNext(0) must beFalse

  def scs2 = SeqCommandStream[Int, Int](List(1, 2)).hasNext(randomInt()) must beTrue

  def scs3 = SeqCommandStream[Int, Int](List(1, 2)).next(randomInt()) must_== (1, SeqCommandStream[Int, Int](List(2)))

  def scs4 = {
    val s = SeqCommandStream[Int, Int](List(1, 2, 3))
    streamDrainerIterator(s, (0 to 4).map(x => randomInt())).toSeq must_== Seq(1, 2, 3)
  }

  def scs5 = SeqCommandStream[Int, Int](Nil).next(randomInt()) must throwA[NoSuchElementException]

  def all1 = {
    val s = SeqCommandStream[Int, Int](List(1, 2, 3))
    val p = new PartialFunctionCommandStream[Int, Int]({
      case 4 => 13
      case 5 => 17
    })
    streamDrainerIterator(s followedBy p, (1 to 10)).toSeq must_== Seq(1, 2, 3, 13, 17)
  }

  case class chainedStream() extends Mockito {
    val s1 = mock[CommandStream[Int, Int]]
    val s2 = mock[CommandStream[Int, Int]]

    val s1n = mock[CommandStream[Int, Int]]
    val s2n = mock[CommandStream[Int, Int]]
    s1.next(any) returns ((123, s1n))
    s2.next(any) returns ((456, s2n))

    val composed = ChainedCommandStream(s1, s2)

    def e1 = {
      s1.hasNext(any) returns true
      s2.hasNext(any) returns false
      (composed.next(randomInt()) must_== (123, ChainedCommandStream(s1n, s2))) and
        (there was one(s1).hasNext(any))
    }

    def e2 = {
      s1.hasNext(any) returns false
      s2.hasNext(any) returns true
      (composed.next(randomInt()) must_== (456, s2n)) and
        (there was one(s1).hasNext(any))
    }

    def e3 = {
      s1.hasNext(any) returns false
      s2.hasNext(any) returns false
      (composed.hasNext(randomInt()) must_== false) and
        (there was one(s1).hasNext(any) then one(s2).hasNext(any))
    }

    def e4 = {
      s1.hasNext(any) returns false
      s2.hasNext(any) returns false
      s2.next(any) throws new NoSuchElementException("Empty stream")
      composed.next(randomInt()) must throwA[NoSuchElementException]
    }
  }

  case class partialStream() extends Mockito {
    val mpf = mock[PartialFunction[Int, Int]]
    val s = new PartialFunctionCommandStream[Int, Int](mpf)
    val pf = new PartialFunctionCommandStream[Int, Int]({
      case 123 => 321
    })

    def e1 = {
      mpf.isDefinedAt(123) returns true
      (s.hasNext(123) must beTrue) and (there was one(mpf).isDefinedAt(123))
    }

    def e2 = {
      val i = randomInt()
      mpf.isDefinedAt(any) returns false
      (s.hasNext(i) must beFalse) and (there was one(mpf).isDefinedAt(i))

    }

    def e3 = {
      (pf.next(123) must_== (321, pf))
    }

    def e4 = {
      pf.next(10) must throwA[NoSuchElementException]
    }
  }

}