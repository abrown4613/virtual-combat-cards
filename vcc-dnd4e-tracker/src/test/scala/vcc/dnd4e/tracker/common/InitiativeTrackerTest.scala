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
package vcc.dnd4e.tracker.common

import org.specs2.SpecificationWithJUnit
import org.specs2.specification.{Fragments, Example, Fragment}

class InitiativeTrackerTest extends SpecificationWithJUnit {

  import InitiativeState._
  import InitiativeAction._

  val ioa = InitiativeOrderID(CombatantID("A"), 0)
  val iob = InitiativeOrderID(CombatantID("A"), 1)
  //Just to make sure we use the InitiativeOrderID not the CombatantID

  val firstTracker = InitiativeState.values.map(st => InitiativeTracker(iob, 0, 0, st)).toList
  val firstMap = Map(firstTracker.map(it => it.state -> it).toSeq: _*)

  def runAllCases(it: InitiativeTracker, trans: Map[(InitiativeTracker, InitiativeAction.Value), InitiativeTracker]): Seq[Fragment] = {
    for (first <- it :: firstTracker; action <- InitiativeAction.values) yield {
      val pair = (first, action)
      val firstp = (it.orderID == first.orderID)
      if (trans.isDefinedAt(pair)) {
        "change to " + trans(pair) + " after doing " + action + " when " + (if (firstp) "it is the first" else "first is " + first.state) ! {
          it.canTransform(first, action) must beTrue
          it.transform(first, action) must_== trans(pair)
        }
      } else {
        "not do " + action + " when " + (if (firstp) "it is the first" else ("first is " + first.state)) ! {
          it.canTransform(first, action) must beFalse
        }
      }
    }
  }

  def is =
    "Waiting tracker should " ^ {
      val it = InitiativeTracker(ioa, 0, 0, InitiativeState.Waiting)

      runAllCases(it, Map(
        (it, StartRound) -> InitiativeTracker(ioa, 1, 0, Acting)
      ))
    } ^ endp ^ "Readying tracker" ^ {
      val it = InitiativeTracker(ioa, 0, 0, Readying)

      runAllCases(it, Map(
        (it, EndRound) -> InitiativeTracker(ioa, 0, 0, Ready)
      ))
    } ^ endp ^ "Ready tracker" ^ {
      val it = InitiativeTracker(ioa, 0, 0, Ready)

      runAllCases(it, Map(
        (it, StartRound) -> InitiativeTracker(ioa, 1, 0, Acting),
        (firstMap(Acting), ExecuteReady) -> InitiativeTracker(ioa, 0, 0, Waiting)
      ))
    } ^ endp ^ "Acting tracker" ^ {
      val it = InitiativeTracker(ioa, 0, 0, Acting)

      runAllCases(it, Map(
        (it, EndRound) -> InitiativeTracker(ioa, 0, 0, Waiting),
        (it, ReadyAction) -> InitiativeTracker(ioa, 0, 0, Readying),
        (it, DelayAction) -> InitiativeTracker(ioa, 0, 0, Delaying)
      ))
    } ^ endp ^ "Delaying tracker" ^ {
      var it = InitiativeTracker(ioa, 0, 0, Delaying)
      runAllCases(it, Map(
        (it, EndRound) -> InitiativeTracker(ioa, 0, 0, Waiting),
        (firstMap(Delaying), MoveUp) -> InitiativeTracker(ioa, 0, 0, Acting),
        (firstMap(Ready), MoveUp) -> InitiativeTracker(ioa, 0, 0, Acting),
        (firstMap(Waiting), MoveUp) -> InitiativeTracker(ioa, 0, 0, Acting)
      ))
    } ^ end
}