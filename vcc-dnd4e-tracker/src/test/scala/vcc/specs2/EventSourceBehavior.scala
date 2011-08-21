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
package vcc.specs2

import org.specs2.execute.{Error, Result}
import org.specs2.execute.Error.ThrowableException
import org.specs2.matcher.{Matcher}
import org.specs2.{SpecificationWithJUnit, Specification}

trait EventSourceBehavior[S, E, C] {
  self: Specification =>
  //self: MustExpectations =>

  /**
   * Build state form a initial and a service of domain events.
   */
  def given(s: S, evt: E*)(implicit buildState: (S, Seq[E]) => S): Given = {
    val result: Either[S, Result] = try {
      Left(buildState(s, evt))
    } catch {
      case e => Right(new Error("Failed to build given state", new ThrowableException(e)))
    }
    new Given(result)
  }

  class Given(stateOrError: Either[S, Result]) {
    /**
     * Execute a command.
     */
    def when(cmd: C)(implicit runner: (S, C) => Seq[E]) = new GivenWithWhen(stateOrError, cmd, runner)
  }

  class GivenWithWhen(stateOrError: Either[S, Result], cmd: C, runner: (S, C) => Seq[E]) {

    /**
     * Check if the command generates the appropriate result
     */
    def then(expected: E*): Result = then(be_==(expected))

    /**
     * Check resulting
     */
    def then(matcher: Matcher[Iterable[E]]): Result = {
      stateOrError match {
        case Left(state) => runner(state, cmd) must matcher
        case Right(r) => r
      }
    }

    /**
     * Check if the When produced a given exception:
     * @param e Expected exception for the when command.
     */
    def failWith[T <: Throwable](e: T): Result = {
      stateOrError match {
        case Left(state) =>
          runner(state, cmd) must throwAn(e)
        case Right(r) => r
      }
    }
  }

}

class EventSourceBehaviorTest extends SpecificationWithJUnit with EventSourceBehavior[Int, Int, String] {

  implicit val builder: (Int, Seq[Int]) => Int = {
    (is, evt) =>
      evt.foldLeft(is)((s, e) => if (e > 0) (s + e) else throw new Exception(e + " not positive"))
  }

  implicit val runTest: (Int, String) => Seq[Int] = {
    (state, command) =>
      command.split(",").map(_.toInt)

  }

  def is =
    "EventSourceBehavior" ^
      "given 0 and 1,2,3 when '4,5' then 4,5" ! given(0, 1, 2, 3).when("4,5").then(4, 5) ^
      "given 0 and 1,2,3 when '4,5' then 4,5 (matcher)" ! given(0, 1, 2, 3).when("4,5,6").then(contain(5, 6).inOrder) ^
      "given 0 and 1,2,3 when '4,a' failWith()" ! given(0, 1, 2, 3).when("4,a").failWith(new NumberFormatException("For input string: \"a\"")) ^
      "given 0 and 1,-2 when '4' then 4" ! e1 ^
      end

  def e1 = {
    val result = (given(0, 1, -2).when("4").then(4))
    (result.isError must beTrue) and (result.message must_== "Failed to build given state")
  }
}