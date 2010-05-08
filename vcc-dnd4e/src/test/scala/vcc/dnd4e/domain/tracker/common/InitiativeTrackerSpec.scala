/**
 * Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.common


import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}

@RunWith(classOf[JUnitSuiteRunner])
class InitiativeTrackerTest extends JUnit4(InitiativeTrackerSpec)

object InitiativeTrackerSpec extends Specification {
  import InitiativeTracker._
  val ioa = InitiativeOrderID(CombatantID("A"), 0)
  val iob = InitiativeOrderID(CombatantID("A"), 1) //Just to make sure we use the InitiativeOrderID not the CombatantID 

  val firstTracker = state.map(st => InitiativeTracker(iob, 0, 0, st)).toList
  val firstMap = Map(firstTracker.map(it => it.state -> it).toSeq: _*)

  def runAllCases(it: InitiativeTracker, trans: Map[(InitiativeTracker, action.Value), InitiativeTracker]) {
    for (first <- it :: firstTracker; action <- InitiativeTracker.action) {
      val pair = (first, action)
      val firstp = (it.orderID == first.orderID)
      if (trans.isDefinedAt(pair)) {
        "change to " + trans(pair) + " after doing " + action + " when " + (if (firstp) "it is the first" else "first is " + first.state) in {
          it.canTransform(first, action) must beTrue
          it.transform(first, action) must_== trans(pair)
        }
      } else {
        "not do " + action + " when " + (if (firstp) "it is the first" else ("first is " + first.state)) in {
          it.canTransform(first, action) must beFalse
        }
      }
    }
  }

  "Waiting tracker" should {
    var it = InitiativeTracker(ioa, 0, 0, state.Waiting)

    runAllCases(it, Map(
      (it, action.StartRound) -> InitiativeTracker(ioa, 1, 0, state.Acting)
      ))
  }

  "Readying tracker" should {
    var it = InitiativeTracker(ioa, 0, 0, state.Readying)

    runAllCases(it, Map(
      (it, action.EndRound) -> InitiativeTracker(ioa, 0, 0, state.Ready)
      ))
  }

  "Ready tracker" should {
    var it = InitiativeTracker(ioa, 0, 0, state.Ready)

    runAllCases(it, Map(
      (it, action.StartRound) -> InitiativeTracker(ioa, 1, 0, state.Acting),
      (firstMap(state.Acting), action.ExecuteReady) -> InitiativeTracker(ioa, 0, 0, state.Waiting)
      ))
  }

  "Acting tracker" should {
    var it = InitiativeTracker(ioa, 0, 0, state.Acting)

    runAllCases(it, Map(
      (it, action.EndRound) -> InitiativeTracker(ioa, 0, 0, state.Waiting),
      (it, action.Ready) -> InitiativeTracker(ioa, 0, 0, state.Readying),
      (it, action.Delay) -> InitiativeTracker(ioa, 0, 0, state.Delaying)
      ))
  }

  "Delaying tracker" should {
    var it = InitiativeTracker(ioa, 0, 0, state.Delaying)
    runAllCases(it, Map(
      (it, action.EndRound) -> InitiativeTracker(ioa, 0, 0, state.Waiting),
      (firstMap(state.Delaying), action.MoveUp) -> InitiativeTracker(ioa, 0, 0, state.Acting),
      (firstMap(state.Ready), action.MoveUp) -> InitiativeTracker(ioa, 0, 0, state.Acting),
      (firstMap(state.Waiting), action.MoveUp) -> InitiativeTracker(ioa, 0, 0, state.Acting)
      ))
  }

}