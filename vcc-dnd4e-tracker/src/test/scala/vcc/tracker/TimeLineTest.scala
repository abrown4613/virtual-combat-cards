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
import util.Random

class TimeLineTest extends SpecificationWithJUnit {
  def is =
    "Timeline".title ^
      "Empty TimeLine" ^
      "  be created empty" ! emptyTimeLine().isEmpty ^
      "  stores data" ! emptyTimeLine().storeData ^
      "  cant move back" ! emptyTimeLine().cantMoveBack ^
      "  cant move forward" ! emptyTimeLine().cantMoveForward ^
      "  has no past" ! emptyTimeLine().hasNoPast ^
      "  has no future" ! emptyTimeLine().hasNoFuture ^
      "  after store has past" ! emptyTimeLine().afterOneStoreHasNoPast ^
      "  after two stores has past" ! emptyTimeLine().afterTwoStoreHasNoPast ^
      endp ^
      "Loaded TimeLine" ^
      "  has past" ! loadedTimeLine(3).hasPast ^
      "  moves to past" ! loadedTimeLine(3).moveToPast ^
      "  moves to past twice" ! loadedTimeLine(3).goBackTwice ^
      "  moves to past has future" ! loadedTimeLine(3).onceMovedToPastHasFuture ^
      "  move to past then forward restores state" ! loadedTimeLine(4).backAndForth ^
      "  move to past twice then return" ! loadedTimeLine(4).backTwiceAndForthTwice ^
      "  going back all the way passes through all state" ! loadedTimeLine(10).travelBackGoesThroughAllStates ^
      "  going back all then forward all the way" ! loadedTimeLine(10).travelBackThenForwardAllTheWay ^
      "  going back and forth and back" ! loadedTimeLine(10).travelBackAndForthAndBack ^
      "  storing truncates future" ! loadedTimeLine(5).storingTruncatesFuture ^
      "  can truncate past" ! loadedTimeLine(5).canTruncatePast ^
      end

  case class emptyTimeLine() {

    private val timeLine = new TimeLine[Int]

    def isEmpty = {
      timeLine.getState must_== None
    }

    def storeData = {
      val n = Random.nextInt()
      timeLine.store(n)
      timeLine.getState must_== Some(n)
    }

    def cantMoveBack = {
      timeLine.revertState() must throwA[TimeLine.OutOfBounds]
    }

    def cantMoveForward = {
      timeLine.forwardState() must throwA[TimeLine.OutOfBounds]
    }

    def hasNoPast = {
      timeLine.hasPast must beFalse
    }

    def hasNoFuture = {
      timeLine.hasFuture must beFalse
    }

    def afterOneStoreHasNoPast = {
      timeLine.store(1)
      timeLine.hasPast must beFalse
    }

    def afterTwoStoreHasNoPast = {
      timeLine.store(1)
      timeLine.store(2)
      timeLine.hasPast must beTrue
    }
  }

  case class loadedTimeLine(size: Int) {

    private val timeLine = new TimeLine[Int]

    private val storedState = (1 to size).map(_ => Random.nextInt()).toList
    storedState.foreach(timeLine.store(_))

    def hasPast = {
      timeLine.hasPast must beTrue
    }

    def moveToPast = {
      timeLine.revertState()
      timeLine.getState must_== storedState.drop(1).headOption
    }

    def onceMovedToPastHasFuture = {
      timeLine.revertState()
      (timeLine.hasFuture must beTrue) and
        (timeLine.getState must_== storedState.reverse.tail.headOption)
    }

    def backAndForth = {
      timeLine.revertState()
      timeLine.forwardState()
      timeLine.getState must_== storedState.reverse.headOption
    }

    def backTwiceAndForthTwice = {
      timeLine.revertState()
      timeLine.revertState()
      timeLine.forwardState()
      timeLine.forwardState()
      timeLine.getState must_== storedState.reverse.headOption
    }

    def goBackTwice = {
      timeLine.revertState()
      timeLine.revertState()
      timeLine.getState must_== storedState.drop(0).headOption
    }

    def travelBackGoesThroughAllStates = {
      val list = travelThroughPast(timeLine)
      list must_== storedState
    }

    def travelBackThenForwardAllTheWay = {
      travelThroughPast(timeLine)
      val list = travelThroughFuture(timeLine)
      list must_== storedState.reverse
    }

    def travelBackAndForthAndBack = {
      travelThroughPast(timeLine)
      travelThroughFuture(timeLine)
      val list = travelThroughPast(timeLine)
      list must_== storedState
    }

    def storingTruncatesFuture = {
      travelThroughPast(timeLine)
      timeLine.store(1)
      timeLine.hasFuture must beFalse
    }

    def canTruncatePast = {
      timeLine.forgetPast()
      timeLine.hasPast must beFalse
    }

    private def travelThroughPast[S](line: TimeLine[S]): List[S] = {
      travel[S](line, _.hasPast, _.revertState())
    }

    private def travelThroughFuture[S](line: TimeLine[S]): List[S] = {
      travel[S](line, _.hasFuture, _.forwardState())
    }

    private def travel[S](timeLine: TimeLine[S], test: (TimeLine[S])=> Boolean, step: (TimeLine[S])=> Unit): List[S] = {
      var l = timeLine.getState.toList
      while(test(timeLine)) {
        step(timeLine)
        l = timeLine.getState.get :: l
      }
      l
    }
  }
}