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
package vcc.tracker

import org.specs2.SpecificationWithJUnit
import org.specs2.mock.Mockito

class CommandStreamTest extends SpecificationWithJUnit {
  def randomInt(): Int = scala.util.Random.nextInt()

  case class ICommand(value: Int) extends Command[Int] {
    override def generateEvents(state: Int): List[Event[Int]] = Nil

    def generateTransitions(iState: Int): List[StateTransition[Int]] = Nil
  }

  implicit def int2ICommand(v: Int):Command[Int] = ICommand(v)

  def streamDrainerIterator[S, A](stream: CommandStream[S, A], states: Seq[S]): Seq[A] = {
    var collect = List.empty[A]
    var rest = states
    var next = stream.get(rest.head)
    while (next.isDefined && !rest.tail.isEmpty) {
      val (item, cs) = next.get
      collect = item :: collect
      rest = rest.tail
      next = cs.get(rest.head)
    }
    collect.reverse
  }

  def is =
    "SeqCommandStream" ^
      "  have defined return if list has content" ! scs2 ^
      "  return first and continuation stream" ! scs3 ^
      "  return all on any sequence" ! scs4 ^
      "  return None on empty sequence" ! scs5 ^
      endp ^
      "ChainedCommandStream" ^
      "  use first stream if it has something to provide" ! chainedStream().e1 ^
      "  use second stream if first does not supply values" ! chainedStream().e2 ^
      "  not have next if both are null" ! chainedStream().e3 ^
      endp ^
      "PartialFunctionCommandStream" ^
      "  return next if defined at he partial function" ! partialStream().e1 ^
      "  return None is pf is not defined at state" ! partialStream().e2 ^
      endp ^
      "all together" ! all1 ^
      "all together - variant" ! all2 ^
      end

  def scs2 = CommandStream[Int, Command[Int]](1, 2).get(randomInt()).isDefined must beTrue

  def scs3 = CommandStream[Int, Command[Int]](1, 2).get(randomInt()) must_== Some((ICommand(1), CommandStream[Int, Command[Int]](2)))

  def scs4 = {
    val s = CommandStream[Int, Command[Int]](1, 2, 3)
    streamDrainerIterator(s, (0 to 4).map(x => randomInt())) must_== Seq[Command[Int]](1, 2, 3)
  }

  def scs5 = SeqCommandStream[Int, Command[Int]](Nil).get(randomInt()) must_== None

  def all1 = {
    val s = CommandStream[Int, Command[Int]](1, 2, 3)
    val p = new PartialFunctionCommandStream[Int, Command[Int]]({
      case 4 => 13
      case 5 => 17
      case 8 => 311
    })
    streamDrainerIterator(s followedBy p, (1 to 10)) must_== Seq[Command[Int]](1, 2, 3, 13, 17)
  }

  def all2 = {
    val s = CommandStream[Int, Command[Int]](1, 2, 3)
    val s2 = CommandStream[Int, Command[Int]](4, 5, 6, 7)
    val p = new PartialFunctionCommandStream[Int, Command[Int]]({
      case 4 => 13
      case 5 => 17
      case 8 => 311
    })
    val c: CommandStream[Int, Command[Int]] = s followedBy s2 followedBy p
    (1 to 10).foldLeft((Option(c), List.empty[Command[Int]]))((sp, s) => sp._1.map(stream => stream.get(s) match {
      case Some((v, str)) => (Some(str), v :: sp._2)
      case None => (None, sp._2.reverse)
    }).getOrElse((None, sp._2)))._2 must_== Seq[Command[Int]](1, 2, 3, 4, 5, 6, 7, 311)
  }

  case class chainedStream() extends Mockito {
    val s1 = mock[CommandStream[Int, Command[Int]]]
    val s2 = mock[CommandStream[Int, Command[Int]]]

    val s1n = mock[CommandStream[Int, Command[Int]]]
    val s2n = mock[CommandStream[Int, Command[Int]]]
    s1.get(any) returns (Some((123, s1n)))
    s2.get(any) returns (Some((456, s2n)))

    val composed = ChainedCommandStream(s1, s2)

    def e1 = {
      s2.get(any) returns (None)
      (composed.get(randomInt()) must_== Some((ICommand(123), ChainedCommandStream(s1n, s2)))) and
        (there was one(s1).get(any))
    }

    def e2 = {
      s1.get(any) returns (None)
      (composed.get(randomInt()) must_== Some((ICommand(456), s2n))) and
        (there was one(s1).get(any))
    }

    def e3 = {
      s1.get(any) returns (None)
      s2.get(any) returns (None)
      (composed.get(randomInt()) must_== None) and
        (there was one(s1).get(any) then one(s2).get(any))
    }
  }

  case class partialStream() {
    val pf = new PartialFunctionCommandStream[Int, Command[Int]]({
      case 123 => 321
    })

    def e1 = {
      (pf.get(123) must_== Some((ICommand(321), pf)))
    }

    def e2 = {
      pf.get(10) must_== None
    }
  }
}