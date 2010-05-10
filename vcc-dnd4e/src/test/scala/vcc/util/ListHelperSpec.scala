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

import ListHelper._
import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}

@RunWith(classOf[JUnitSuiteRunner])
class ListHelperTest extends JUnit4(ListHelperSpec)

object ListHelperSpec extends Specification {
  "splice list" should {
    "replace first element" in {splice(List(1, 2, 3, 4, 5), 0, List(10, 11)) must_== List(10, 11, 2, 3, 4, 5)}
    "replace second element" in {splice(List(1, 2, 3, 4, 5), 1, List(20)) must_== List(1, 20, 3, 4, 5)}
    "replace third element" in {splice(List(1, 2, 3, 4, 5), 2, List(30)) must_== List(1, 2, 30, 4, 5)}
    "replace last element" in {splice(List(1, 2, 3, 4, 5), 4, List(50)) must_== List(1, 2, 3, 4, 50)}
    "replace last element" in {splice(List(1, 2, 3, 4, 5), 4, List(50, 51)) must_== List(1, 2, 3, 4, 50, 51)}
    "fail on out of range" in {splice(List(1, 2, 3, 4, 5), 6, List(50)) must throwA[Exception]}
    "fail on out of range" in {splice(List(1, 2, 3, 4, 5), -1, List(50)) must throwA[Exception]}
  }


}