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
package vcc.infra.util

import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}

@RunWith(classOf[JUnitSuiteRunner])
class RoundRobinTest extends JUnit4(RoundRobinSpec)

object RoundRobinSpec extends Specification {
  var roundRobin: RoundRobin[Symbol] = null

  "a empty RoundRobin" ->- (beforeContext {roundRobin = new ArrayRoundRobin(null, Seq())}) should {
    "return None for head" in {roundRobin.headOption must_== None}
    "fail to advanceTo" in {roundRobin.advanceTo('A) must throwA[NoSuchElementException]}
  }

  "ArrayRoundRobin builder" should {
    "fail to create a robin with a head that is not in the list" in {
      new ArrayRoundRobin[Int](10, Nil) must throwA[NoSuchElementException]
    }
  }

  "a full RoundRobin" ->- (beforeContext {roundRobin = new ArrayRoundRobin('B, Seq('A, 'B, 'C, 'D))}) should {

    "return head" in {roundRobin.headOption must_== Some('B)}

    "fail to advanceTo to someone not in array" in {roundRobin.advanceTo('E) must throwA[NoSuchElementException]}

    "move head to a valid position before" in {
      roundRobin.advanceTo('A)
      roundRobin.headOption must_== Some('A)
    }

    "move head to a valid position after" in {
      roundRobin.advanceTo('D)
      roundRobin.headOption must_== Some('D)
    }

    "advance" in {
      roundRobin.advance()
      roundRobin.headOption must_== Some('C)
    }

    "advance beyond end position after" in {
      roundRobin.advanceTo('D)
      roundRobin.advance()
      roundRobin.headOption must_== Some('A)
    }

    "after clearing a new definition will with not head maintains order - issue 197" in {
      roundRobin.headOption must_== Some('B)
      roundRobin.setRobin(null, Seq('A, 'B, 'C, 'D))
      roundRobin.apply(0) must_== 'A

    }
  }
  "a full RoundRobin without explicit head " ->- (beforeContext {
    roundRobin = new ArrayRoundRobin(null, Seq('A, 'B))
  }) should {
    "have a head" in {
      roundRobin.headOption must_== Some('A)
    }
    "advance normally" in {
      roundRobin.advance()
      roundRobin.headOption must_== Some('B)
    }

    "wrap around" in {
      roundRobin.advance()
      roundRobin.advance()
      roundRobin.headOption must_== Some('A)
    }

    "wrap around and advance " in {
      roundRobin.advance()
      roundRobin.advance()
      roundRobin.advance()
      roundRobin.headOption must_== Some('B)
    }

    "provide a linear read from a advanced head" in {
      roundRobin.advance()
      roundRobin.length must_== 2
      roundRobin(0) must_== 'B
      roundRobin(1) must_== 'A
      roundRobin(2) must throwAn[IndexOutOfBoundsException]
    }
  }
}