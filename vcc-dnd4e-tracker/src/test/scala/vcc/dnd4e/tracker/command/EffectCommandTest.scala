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
package vcc.dnd4e.tracker.command

import org.specs2.SpecificationWithJUnit
import vcc.dnd4e.tracker.common.Effect.Condition
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.event._
import vcc.tracker.IllegalActionException

class EffectCommandTest extends SpecificationWithJUnit with CombatStateEventSourceBehavior with EventSourceSampleEvents {
  private val eid = EffectID(combA, 11)
  private val eidBroken = EffectID(combB, 0)

  private val buildEvents = Seq(evtAddCombA, evtAddComb2, evtAddCombNoId,
    meAddToOrder(combA, 5, 15), meAddToOrder(comb1, 4, 14), meAddToOrder(comb2, 3, 13), evtStart)
  private val addEffectTransition = AddEffectCommand(combA, combB, Condition.Generic("nice", true), Duration.RoundBound(ioA0, Duration.Limit.EndOfTurnSustain))

  def is =
    "Effect transition test".title ^
      execAddEffect ^
      execCancelEffectTransition ^
      execSustainEffectTransition ^
      execUpdateEffectTransition ^
      end

  private val conditionGood = Condition.Generic("nice", true)
  private val conditionBad = Condition.Generic("bad", false)
  private val durationEoRA0 = Duration.RoundBound(ioA0, Duration.Limit.EndOfTurnSustain)

  private def execAddEffect = {
    val addedEffectEvent = AddEffectEvent(combA, combB, conditionGood, durationEoRA0)
    val brokenEffectTransition = AddEffectCommand(combB, combB, conditionGood, durationEoRA0)
    "Add Effect" ^
      "add effect" !
        (given(emptyState, buildEvents) when (addEffectTransition) then (addedEffectEvent)) ^
      "fail if target not defined" !
        (given(emptyState, buildEvents) when (brokenEffectTransition) must (throwAn[IllegalActionException])) ^
      endp
  }

  private def execCancelEffectTransition = {
    val expectedEvent = ChangeEffectListEvent(eid.combId, EffectTransformation.cancelEffect(eid))
    val goodTransition = CancelEffectCommand(eid)
    val badTransition = CancelEffectCommand(eidBroken)
    "Cancel Efect" ^
      "do cancel effect" !
        (given(emptyState, buildEvents) when (goodTransition) then (expectedEvent)) ^
      "fail because target not defined" !
        (given(emptyState, buildEvents) when (badTransition) must (throwAn[IllegalActionException])) ^
      endp
  }

  private def execSustainEffectTransition = {
    val expectedEvent = ChangeEffectListEvent(eid.combId, EffectTransformation.sustainEffect(eid))
    val goodTransition = SustainEffectCommand(eid)
    val badTransition = SustainEffectCommand(eidBroken)
    "Sustain Efect" ^
      "do cancel effect" !
        (given(emptyState, buildEvents) when (goodTransition) then (expectedEvent)) ^
      "fail because target not defined" !
        (given(emptyState, buildEvents) when (badTransition) must (throwAn[IllegalActionException])) ^
      endp
  }

  private def execUpdateEffectTransition = {
    val expectedEvent = ChangeEffectListEvent(eid.combId, EffectTransformation.updateCondition(eid, conditionBad))
    val goodTransition = UpdateEffectConditionCommand(eid, conditionBad)
    val badTransition = UpdateEffectConditionCommand(eidBroken, conditionBad)
    "Update Efect" ^
      "do cancel effect" !
        (given(emptyState, buildEvents) when (goodTransition) then (expectedEvent)) ^
      "fail because target not defined" !
        (given(emptyState, buildEvents) when (badTransition) must (throwAn[IllegalActionException])) ^
      endp
  }
}