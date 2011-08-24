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
package vcc.dnd4e.tracker.transition

import org.specs2.SpecificationWithJUnit
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.event._
import vcc.controller.IllegalActionException

class InitiativeTransitionTest extends SpecificationWithJUnit with CombatStateEventSourceBehavior with EventSourceSampleEvents {
  def is =
    "InitiativeTransition".title ^
      mustStart ^
      mustEndRound ^
      execDelay ^
      execEndRoundOfDelaying ^
      execMoveUp ^
      execReady ^
      execExecuteReady ^
      execMoveBefore ^
      end

  private val buildEvents = Seq(
    evtAddCombA, evtAddComb2, evtAddCombNoId,
    meAddToOrder(combA, 5, 10), meAddToOrder(comb1, 1, 10), meAddToOrder(comb2, 1, 1),
    evtStart)
  private val smallCombatBuildEvents = Seq(
    evtAddCombA, evtAddCombNoId, meAddToOrder(comb1, 1, 1), meAddToOrder(combA, 5, 10), evtStart)

  private val evtStartRoundA = InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.StartRound)
  private val evtDelayRoundA = InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.DelayAction)
  private val evtReadyRoundA = InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.ReadyAction)
  private val evtEndRoundA = InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.EndRound)
  private val evtStartRound1 = InitiativeTrackerUpdateEvent(io1_0, InitiativeAction.StartRound)
  private val evtEndRound1 = InitiativeTrackerUpdateEvent(io1_0, InitiativeAction.EndRound)

  private def mustStart = {
    "Start round" ^
      "fail if not defined" !
        (given(emptyState, buildEvents) when (StartRoundTransition(ioB0)) must throwAn[IllegalActionException]) ^
      "start round" !
        (given(emptyState, buildEvents).
          when(StartRoundTransition(ioA0)).
          then(InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.StartRound), EffectListTransformEvent(EffectTransformation.startRound(ioA0)))) ^
      endp
  }

  private def mustEndRound = {
    "do End round" ^
      "fail if not defined" !
        (given(emptyState, buildEvents, evtStartRoundA) when (EndRoundTransition(ioB0)) must throwAn[IllegalActionException]) ^
      "end round and rotate" !
        (given(emptyState, buildEvents).
          when(EndRoundTransition(ioA0)).
          then(InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.EndRound), RotateRobinEvent, EffectListTransformEvent(EffectTransformation.endRound(ioA0)))) ^
      endp
  }

  private def execDelay = {
    "do Delay round" ^
      "fail if not defined" !
        (given(emptyState, buildEvents, evtStartRoundA) when (DelayTransition(ioB0)) must throwAn[IllegalActionException]) ^
      "delay and rotate" !
        (given(emptyState, buildEvents, evtStartRoundA).
          when(DelayTransition(ioA0)).
          then(InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.DelayAction), RotateRobinEvent, DelayEffectListTransformEvent(ioA0))) ^
      endp
    //TODO Check for illegal state before execute
  }

  private def execEndRoundOfDelaying = {
    "do end round of delaying" !
      (given(emptyState, smallCombatBuildEvents, evtStartRoundA, evtDelayRoundA, RotateRobinEvent, evtStartRound1, evtEndRound1, RotateRobinEvent).
        when(EndRoundTransition(ioA0)).
        then(InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.EndRound), EffectListTransformEvent(EffectTransformation.endRound(ioA0))))
  }

  private def execMoveUp = {
    "do Move Up round" ^
      "fail if not defined" !
        (given(emptyState, buildEvents, evtStartRoundA, evtDelayRoundA, RotateRobinEvent) when (MoveUpTransition(ioB0)) must throwAn[IllegalActionException]) ^
      "move up, move to first, and set robin" !
        (given(emptyState, buildEvents, evtStartRoundA, evtDelayRoundA, RotateRobinEvent).
          when(MoveUpTransition(ioA0)).
          then(InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.MoveUp), MoveBeforeFirstEvent(ioA0), SetRobinEvent(ioA0))) ^
      endp

  }

  private def execReady = {
    "do Ready action" ^
      "fail if not defined" !
        (given(emptyState, buildEvents, evtStartRoundA) when (ReadyActionTransition(ioB0)) must throwAn[IllegalActionException]) ^
      "delay and rotate" !
        (given(emptyState, buildEvents, evtStartRoundA).
          when(ReadyActionTransition(ioA0)).
          then(InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.ReadyAction))) ^
      endp

  }

  private def execExecuteReady = {
    val events = buildEvents ++ Seq(evtStartRoundA, evtReadyRoundA, evtEndRoundA, RotateRobinEvent, evtStartRound1)
    "do Move Up round" ^
      "fail if not defined" !
        (given(emptyState, events) when (ExecuteReadyTransition(ioB0)) must throwAn[IllegalActionException]) ^
      "move up, move to first, and set robin" !
        (given(emptyState, events).
          when(ExecuteReadyTransition(ioA0)).
          then(InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.ExecuteReady), MoveBeforeFirstEvent(ioA0))) ^
      endp

  }

  private def execMoveBefore = {
    val exception: IllegalActionException = new IllegalActionException("Cant move " + ioA0 + " before " + io2_0)
    "do Move Before" ^
      "fail if not defined" !
        (given(emptyState, buildEvents) when (MoveBeforeTransition(ioA0, ioB0)) must throwAn[IllegalActionException]) ^
      "fail if not defined" !
        (given(emptyState, buildEvents) when (MoveBeforeTransition(ioB0, ioA0)) must throwAn[IllegalActionException]) ^
      "fail if not allowed" !
        (given(emptyState, buildEvents, evtStartRoundA) when (MoveBeforeTransition(ioA0, io2_0)) failWith exception) ^
      "moving first out should move rotate then move" ! //TODO This will deprecate with new always acting
        (given(emptyState, buildEvents).
          when(MoveBeforeTransition(ioA0, io2_0)).
          then(RotateRobinEvent, MoveBeforeOtherEvent(ioA0, io2_0))) ^
      "moving any but the first is simple" !
        (given(emptyState, buildEvents).
          when(MoveBeforeTransition(io2_0, io1_0)).
          then(MoveBeforeOtherEvent(io2_0, io1_0))) ^
      endp
  }
}