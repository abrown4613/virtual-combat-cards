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
import java.lang.Thread
import concurrent.SyncVar

class TrackerTest extends SpecificationWithJUnit {

  private class ThreadedObserver[S] extends Tracker.Observer[S] {
    private val value = new SyncVar[S]

    def getPing: Option[S] = value.get(70)

    def stateUpdated(newState: S) {
      value.set(newState)
    }
  }

  def is =
    "ThreadedObserver" ^
      "get and answer in time out range" ! threadObserver().answerComeWithinTimeLimit ^
      "fails if anwer does not come quick enough" ! threadObserver().answerDoesNotComeWithTimeLimit ^
      endp ^
      "Tracker" ^
      "  notify single observer" ! notificationCases().roundTripToSingleObserver ^
      "  nofify two observer" ! notificationCases().roundTripToTwoObserver ^
      end


  case class threadObserver() {
    private val to = new ThreadedObserver[Int]()
    private val n = Random.nextInt()

    def answerComeWithinTimeLimit = {
      fireUpdateAfterMillis(20)
      to.getPing must_== Some(n)
    }

    def answerDoesNotComeWithTimeLimit = {
      fireUpdateAfterMillis(100)
      to.getPing must_== None
    }

    private def fireUpdateAfterMillis(i: Int) {
      executeInOtherThread(i) {
        to.stateUpdated(n)
      }
    }
  }

  private case class notificationCases() {
    private val tracker = new Tracker[Int]()
    private val observer = new ThreadedObserver[Int]()
    private val n = Random.nextInt()

    tracker.addObserver(observer)

    def roundTripToSingleObserver = {
      executeInOtherThread(20)(tracker.notifyObservers(n))
      observer.getPing must_== Some(n)
    }

    def roundTripToTwoObserver = {
      val observer2 = new ThreadedObserver[Int]()
      tracker.addObserver(observer2)
      executeInOtherThread(20)(tracker.notifyObservers(n))
      (observer2.getPing must_== Some(n)) and
        (observer.getPing must_== Some(n))
    }
  }

  private def executeInOtherThread(blockPeriod: Long)(block: => Unit) {
    new Thread() {
      override def run() {
        Thread.sleep(blockPeriod)
        block
      }
    }.start()
  }
}