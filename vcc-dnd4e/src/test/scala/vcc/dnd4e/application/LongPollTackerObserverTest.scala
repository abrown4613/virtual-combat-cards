/*
 * Copyright (C) 2008-2012 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.application

import org.specs2.SpecificationWithJUnit
import concurrent.SyncVar
import actors.Actor
import org.specs2.specification.AroundExample
import org.specs2.time.TimeConversions
import org.specs2.execute.Result
import org.specs2.execute.EventuallyResults._

trait RetryExamples extends AroundExample with TimeConversions {

  def around[R <% Result](r: =>R) =
    eventually(retries = 3, sleep = 10.millis)(r)

}

class LongPollTackerObserverTest extends SpecificationWithJUnit with RetryExamples {

  def is = "LongPollTackerObserver".title ^
    "send in time" ! s().e1 ^
    "timeout on wait" ! s().e2 ^
    "one early one late" ! s().e3 ^
    "update n" ! s().e4 ^
    "interleave update with setting" ! s().e5 ^
    end

  case class s() {
    private val obs = new LongPollTrackerObserver[Int]()

    def e1 = {
      val f = new SyncVar[Int]
      obs.waitingForState(f)
      updateIn(100, 10)
      f.get(150) must_== Some(10)
    }

    def e2 = {
      val f = new SyncVar[Int]
      obs.waitingForState(f)
      updateIn(100, 10)
      f.get(50) must_== None
    }

    def e3 = {
      val f = new SyncVar[Int]
      val f2 = new SyncVar[Int]
      obs.waitingForState(f2)
      obs.waitingForState(f)
      updateIn(100, 10)
      val v1 = f.get(60)
      val v2 = f2.get(60)
      (v1 must_== None) and (v2 must_== Some(10))
    }

    def e4 = {
      val svs = (0 to 4).toSeq map( _ => new SyncVar[Int]())
      svs.foreach(obs.waitingForState(_))
      updateIn(100, 12)
      Thread.sleep(120)

      svs.map(_.get) must_== Seq.fill(5)(12)
    }

    def e5 = {
      val f = new SyncVar[Int]
      val f2 = new SyncVar[Int]
      obs.waitingForState(f)
      Actor.actor {
        Thread.sleep(50)
        obs.stateUpdated(13)
        obs.waitingForState(f2)
      }
      val v1 = f.get(60)
      val v2 = f2.get(60)
      (v1 must_== Some(13)) and (v2 must_== None)
    }

    def updateIn(sleep: Int, value: Int) {
      Actor.actor {
        Thread.sleep(sleep)
        obs.stateUpdated(value)
      }
    }
  }

}