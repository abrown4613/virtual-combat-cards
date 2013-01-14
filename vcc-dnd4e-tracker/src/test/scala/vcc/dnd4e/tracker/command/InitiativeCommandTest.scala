/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.tracker.event._
import vcc.tracker.IllegalActionException

class InitiativeCommandTest extends SpecificationWithJUnit with CombatStateEventSourceBehavior with EventSourceSampleEvents {
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

  private def mustStart = {
    "Start round" ^
      "fail if not defined" !
        (given(emptyState, buildEvents) when (StartRoundCommand(ioB0)) must throwAn[IllegalActionException]) ^
      "start round" !
        (given(emptyState, buildEvents).
          when(StartRoundCommand(ioA0)).
          andThen(InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.StartRound), EffectListTransformEvent(EffectTransformation.startRound(ioA0)))) ^
      endp
  }

  private def mustEndRound = {
    "do End round" ^
      "fail if not defined" !
        (given(emptyState, buildEvents, evtStartRoundA) when (EndRoundCommand(ioB0)) must throwAn[IllegalActionException]) ^
      "end round and rotate" !
        (given(emptyState, buildEvents).
          when(EndRoundCommand(ioA0)).
          andThen(InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.EndRound), RotateRobinEvent, EffectListTransformEvent(EffectTransformation.endRound(ioA0)))) ^
      endp
  }

  private def execDelay = {
    "do Delay round" ^
      "fail if not defined" !
        (given(emptyState, buildEvents, evtStartRoundA) when (DelayCommand(ioB0)) must throwAn[IllegalActionException]) ^
      "delay and rotate" !
        (given(emptyState, buildEvents).
          given(StartRoundCommand(ioA0)).
          when(DelayCommand(ioA0)).
          andThen(InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.DelayAction), RotateRobinEvent, DelayEffectListTransformEvent(ioA0))) ^
      endp
  }

  private def execEndRoundOfDelaying = {
    "do end round of delaying" !
      (given(emptyState, smallCombatBuildEvents).
        given(StartRoundCommand(ioA0), DelayCommand(ioA0), StartRoundCommand(io1_0), EndRoundCommand(io1_0)).
        when(EndRoundCommand(ioA0)).
        andThen(InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.EndRound), EffectListTransformEvent(EffectTransformation.endRound(ioA0))))
  }

  private def execMoveUp = {
    "do Move Up round" ^
      "fail if not defined" !
        (given(emptyState, buildEvents, evtStartRoundA, evtDelayRoundA, RotateRobinEvent) when (MoveUpCommand(ioB0)) must throwAn[IllegalActionException]) ^
      "move up, move to first, and set robin" !
        (given(emptyState, buildEvents, evtStartRoundA, evtDelayRoundA, RotateRobinEvent).
          when(MoveUpCommand(ioA0)).
          andThen(InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.MoveUp), MoveBeforeFirstEvent(ioA0), SetRobinEvent(ioA0))) ^
      endp

  }

  private def execReady = {
    "do Ready action" ^
      "fail if not defined" !
        (given(emptyState, buildEvents, evtStartRoundA) when (ReadyActionCommand(ioB0)) must throwAn[IllegalActionException]) ^
      "delay and rotate" !
        (given(emptyState, buildEvents, evtStartRoundA).
          when(ReadyActionCommand(ioA0)).
          andThen(InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.ReadyAction))) ^
      endp

  }

  private def execExecuteReady = {
    val events = buildEvents ++ Seq(evtStartRoundA, evtReadyRoundA, evtEndRoundA, RotateRobinEvent, evtStartRound1)
    "do Move Up round" ^
      "fail if not defined" !
        (given(emptyState, events) when (ExecuteReadyCommand(ioB0)) must throwAn[IllegalActionException]) ^
      "move up, move to first, and set robin" !
        (given(emptyState, events).
          when(ExecuteReadyCommand(ioA0)).
          andThen(InitiativeTrackerUpdateEvent(ioA0, InitiativeAction.ExecuteReady), MoveBeforeFirstEvent(ioA0))) ^
      endp

  }

  private def execMoveBefore = {
    val exception = new IllegalActionException("Cant move " + ioA0 + " before " + io2_0)
    "do Move Before" ^
      "fail if not defined" !
        (given(emptyState, buildEvents) when (MoveBeforeCommand(ioA0, ioB0)) must throwAn[IllegalActionException]) ^
      "fail if not defined" !
        (given(emptyState, buildEvents) when (MoveBeforeCommand(ioB0, ioA0)) must throwAn[IllegalActionException]) ^
      "fail if not allowed" !
        (given(emptyState, buildEvents, evtStartRoundA) when (MoveBeforeCommand(ioA0, io2_0)) failWith exception) ^
      "moving first out should move rotate then move" !
        (given(emptyState, buildEvents).
          when(MoveBeforeCommand(ioA0, io2_0)).
          andThen(RotateRobinEvent, MoveBeforeOtherEvent(ioA0, io2_0))) ^
      "moving any but the first is simple" !
        (given(emptyState, buildEvents).
          when(MoveBeforeCommand(io2_0, io1_0)).
          andThen(MoveBeforeOtherEvent(io2_0, io1_0))) ^
      endp
  }
}