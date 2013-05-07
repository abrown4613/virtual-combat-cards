/*
 * Copyright (C) 2013-2013 - Thomas Santana <tms@exnebula.org>
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
package org.exnebula.iteratee

import org.specs2.SpecificationWithJUnit
import org.specs2.matcher.Matcher

class IterateeTest extends SpecificationWithJUnit {

  def is = {
    "Iteratee".title ^
      "base map and flat map" ^
      "  map" ! mapWorks ^
      composingCases ^ endp ^
      "alternate" ^ alternateCases ^ endp ^
      "repeat groups" ^ repeatCases ^ endp ^
      "optional case" ^ optionalCases ^ endp ^
      end
  }

  // Matching
  private def haveCorrectErrorMessage[I, T](e: String) =
    (be_==(e) ^^ { t: (Either[Throwable, T], List[I]) => t._1.left.get.getMessage}).updateMessage("incorrect message: " + _)

  private def beErrorResult[I, T] =
    (beLeft ^^ { t: (Either[Throwable, T], List[I]) => t._1}).updateMessage("not error result: " + _)

  private def beCompletionResult[I, T] =
    (beRight ^^ { t: (Either[Throwable, T], List[I]) => t._1}).updateMessage("not completion result: " + _)

  private def haveCorrectResult[I, T](expectedValue: T) =
    (be_==(expectedValue) ^^ { t: (Either[Throwable, T], List[I]) => t._1.right.get}).updateMessage("incorrect result: " + _)

  private def haveCorrectRemainder[I, T](r: List[I]) =
    (be_==(r) ^^ { t: (Either[Throwable, T], List[I]) => t._2}).updateMessage("incorrect remainder: " + _)

  private def beRejected[I, T](expectedError: String, expectedRemainder: List[I]) =
    beErrorResult[I, T] and haveCorrectErrorMessage[I, T](expectedError) and haveCorrectRemainder[I, T](expectedRemainder)

  private def beAccepted[I, T](expectedValue: T, expectedRemainder: List[I]) =
    beCompletionResult[I, T] and haveCorrectResult[I, T](expectedValue) and haveCorrectRemainder[I, T](expectedRemainder)

  private val isNumber = ("""(\d+)""".r)
  private val matchNumber = new PFConsumer[String, Int]({
    case isNumber(s) => s.toInt
  })

  private val matchFoo = matchConsumer("Foo")
  private val matchBar = matchConsumer("Bar")
  private val matchFooNumber = for {
    x <- matchFoo
    y <- matchNumber
  } yield (x, y)

  private val matchFooBarWithoutConsuming = for {
    x <- matchFoo
    y <- matchButDoNotConsume("Bar")
  } yield (x, y)

  private val matchConsumeAndInject = new PFConsumer2[Int, Int]({
    case n: Int => Done(n + 1, Chunk(n + 2))
  })

  private val matchFooOrBar = matchFoo orElse matchBar

  private val matchFooNumbers = for {
    f <- matchFoo
    ns <- repeat(matchNumber)
  } yield (f, ns)

  private val matchBarNil = for {
    f <- matchBar
  } yield (f, Nil.asInstanceOf[List[Int]])

  private val matchComposed = for {
    blocks <- matchFooNumbers
    f <- matchConsumer("B")
  } yield (blocks, f)

  private val matchBarThenOptionalNumberThenFoo = for {
    _ <- matchBar
    opt <- optional(matchNumber)
    _ <- matchFoo
  } yield opt

  private val matchOptionalBarNotConsumeThenBar = for {
    bar1 <- optional(matchButDoNotConsume("Bar"))
    bar2 <- matchBar
  } yield bar1.isDefined

  private val composingCases = Seq(
    accept("consume all", matchFooNumber).consuming("Foo", "10").yielding(("Foo", 10)).noRest,
    accept("consume and leave rest", matchFooNumber).consuming("Foo", "1", "rest").yielding(("Foo", 1)).remaining("rest"),
    accept("last consumer does not use input", matchFooBarWithoutConsuming).
      consuming("Foo", "Bar", "rest").yielding(("Foo", "Bar")).remaining("Bar", "rest"),
    accept("Consume and inject", matchConsumeAndInject).consuming(1, 9).yielding(2).remaining(3, 9),
    reject("Mismatch on first", matchFooNumber).consuming("Baz").reporting("not matched Baz").remaining("Baz"),
    reject("Mismatch on second", matchFooNumber).consuming("Foo", "Baz").reporting("not matched Baz").remaining("Baz"),
    reject("No output on EOF", matchFooNumber).consuming().reporting("Unexpected end").remaining()
  )

  private val alternateCases = Seq(
    accept("match first consumer", matchFooOrBar).consuming("Foo", "10").yielding("Foo").remaining("10"),
    accept("match second consumer", matchFooOrBar).consuming("Bar", "10").yielding("Bar").remaining("10"),
    reject("not match any", matchFooOrBar).consuming("Baz", "10").reporting("not matched Baz").remaining("Baz", "10"),
    reject("No output on EOF", matchFooOrBar).consuming().reporting("Unexpected end").remaining()
  )

  private val repeatCases = Seq(
    accept("simple repeat", repeat(matchNumber)).consuming("1", "2", "3").yielding(List(1, 2, 3)).remaining(),
    accept("simple repeat with left over", repeat(matchNumber)).consuming("1", "2", "B").yielding(List(1, 2)).remaining("B"),
    accept("repeat group", matchFooNumbers).consuming("Foo", "3", "4", "B").yielding(("Foo", List(3, 4))).remaining("B"),
    accept("nested repeats", repeat(matchFooNumbers)).consuming("Foo", "3", "4", "Foo", "7", "8", "B").
      yielding(List(("Foo", List(3, 4)), ("Foo", List(7, 8)))).remaining("B"),
    accept("error break repeat", repeat(matchFooNumbers)).consuming("Foo", "3", "4", "Bar", "Foo", "7", "8", "B").
      yielding(List(("Foo", List(3, 4)))).remaining("Bar", "Foo", "7", "8", "B"),
    reject("repeat break then error", repeat(matchComposed)).consuming("Foo", "3", "4", "Bar", "Foo", "7", "B").
      reporting("not matched Bar").remaining("Bar", "Foo", "7", "B"),
    accept("nested repeats with alternates", repeat(matchFooNumbers orElse matchBarNil)).
      consuming("Foo", "3", "4", "Bar", "Foo", "7", "8", "B").
      yielding(List(("Foo", List(3, 4)), ("Bar", Nil), ("Foo", List(7, 8)))).remaining("B")
  )

  private val optionalCases = Seq(
    accept("optional does not match", optional(matchNumber)).consuming("a", "1").yielding(None).remaining("a", "1"),
    accept("optional does not match empty", optional(matchNumber)).consuming().yielding(None).remaining(),
    accept("optional matches", optional(matchNumber)).consuming("1", "a").yielding(Some(1)).remaining("a"),
    accept("match in flow ", matchBarThenOptionalNumberThenFoo).
      consuming("Bar", "123", "Foo").yielding(Some(123)).remaining(),
    accept("match in flow ", matchBarThenOptionalNumberThenFoo).
      consuming("Bar", "Foo").yielding(None).remaining(),
    accept("match optional repeat", optional(repeat(matchNumber))).
      consuming("1", "2", "Foo").yielding(Some(List(1,2))).remaining("Foo"),
    accept("match optional empty repeat", optional(repeat(matchNumber))).
      consuming("Foo").yielding(Some(Nil)).remaining("Foo"),
    accept("match optional but not consume", matchOptionalBarNotConsumeThenBar).
      consuming("Bar", "Foo").yielding(true).remaining("Foo"),
    reject("fail to match optional with complex matcher", matchOptionalBarNotConsumeThenBar).
      consuming("Foo").reporting("not matched Foo").remaining("Foo")
  )
  private class RejectedInput[I, T](name: String, consumer: Consumer[I, T]) {
    var input: List[I] = Nil
    var errorMessage: String = null

    def consuming(input: I*) = {
      this.input = input.toList
      this
    }

    def reporting(message: String) = {
      this.errorMessage = message
      this
    }

    def remaining(remainder: I*) = {
      name ! (consumer.consumeAll(input) must beRejected(errorMessage, remainder.toList))
    }
  }

  private def accept[I, T](name: String, consumer: Consumer[I, T]) = new AcceptInput[I, T](name, consumer)

  private class AcceptInput[I, T](name: String, consumer: Consumer[I, T]) {
    var input: List[I] = Nil
    var output: T = _

    def consuming(input: I*) = {
      this.input = input.toList
      this
    }

    def yielding(value: T) = {
      this.output = value
      this
    }

    def remaining(remainder: I*) = {
      name ! (consumer.consumeAll(input) must  beAccepted(output, remainder.toList))
    }

    def noRest = remaining()
  }

  private def reject[I, T](name: String, consumer: Consumer[I, T]) = new RejectedInput[I, T](name, consumer)

  private class PFConsumer[I, T](pf: PartialFunction[I, T]) extends Consumer[I, T] {
    def consume(input: Input[I]): ConsumerState[I, T] = input match {
      case EOF => Error(new Exception("Unexpected end"), EOF)
      case Empty => Continue(this)
      case Chunk(c) => pf.lift(c).map(Done[I, T](_, Empty)).getOrElse(Error(new Exception("not matched " + c), input))
    }
  }

  private class PFConsumer2[I, T](pf: PartialFunction[I, ConsumerState[I, T]]) extends Consumer[I, T] {
    def consume(input: Input[I]): ConsumerState[I, T] = input match {
      case EOF => Error(new Exception("Unexpected end"), EOF)
      case Empty => Continue(this)
      case Chunk(c) => pf.lift(c).getOrElse(Error(new Exception("not matched " + c), input))
    }
  }

  private def matchConsumer[E](name: E) = new PFConsumer[E, E]({
    case n if (n == name) => n
  })

  private def matchButDoNotConsume[E](name: E) = new PFConsumer2[E, E]({
    case n if (n == name) => Done(n, Chunk(n))
  })

  private def mapWorks = {
    val c = for {x <- matchConsumer("Foo")} yield x
    (c.consume(Chunk("Foo")) must_== Done("Foo", Empty)) and
      (c.consume(Chunk("Baz")) must beCorrectError("not matched Baz", Chunk("Baz"))) and
      (c.consume(EOF) must beCorrectError("Unexpected end", EOF))
  }

  def beCorrectError[I, T](errorMessage: String, input: Input[I]): Matcher[ConsumerState[I, T]] = {
    val errorCheck: Matcher[ConsumerState[I, T]] = beLike({ case Error(e, _) => e.getMessage must_== errorMessage})
    val restCheck: Matcher[ConsumerState[I, T]] = (beLike({ case Error(_, i) => i must_== input}))
    errorCheck.updateMessage("incorrect error message: " + _) and
      restCheck.updateMessage("incorrect unprocessed input: " + _)
  }
}