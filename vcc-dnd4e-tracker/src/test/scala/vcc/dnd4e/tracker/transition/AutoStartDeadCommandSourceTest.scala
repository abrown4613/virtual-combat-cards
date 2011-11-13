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
import org.specs2.mock.Mockito
import vcc.dnd4e.tracker.common._
import vcc.scalaz.Lens
import vcc.dnd4e.tracker.event.{CombatStateEvent, AddCombatantEvent, EventSourceSampleEvents}
import vcc.tracker.{Command, CommandStream}

class AutoStartDeadCommandSourceTest extends SpecificationWithJUnit with EventSourceSampleEvents {

  def is =
    "HeadStateAndHealth" ^
      "  extract proper entry on started combat" ! extractor().getsHeadHealthAndInitiativeTrackerWhenCombatStarted ^
      "  extract None if combat not started" ! extractor().doesNotMatchOnceCombatEnded ^
      endp ^
      "Automatic dead handling" ^
      "  auto end round of dead delaying" ! testEndDeadAndDelaying ^
      "  auto start round of dead waiting" ! testStartDeadAndWaiting ^
      "  auto start round of dead ready" ! testStartDeadAndReady ^
      "  auto end round of dead acting" ! testEndDeadAndActing ^
      "  all together" ! testFullDeadAndStartNextAutomation ^
      endp ^
      "Automatic start living" ^
      "  auto start first Waiting" ! testStartWaitingLiving ^
      "  auto start first Ready" ! testStartReadyLiving ^
      "  auto end first Delaying living" ! testEndRoundDelayingLiving ^
      "  on living delay must end and start round" ! testDelayingLivingAutomation ^
      "  if some other is delaying must call for NextUp selection" ! testStartNextUpWithOneDelaying ^
      "  if some other is delaying but dead, must start living" ! testStartNextUpWithOneDelayingButDead ^
      "  if two are delaying must include both on NextUp selection" ! testStartNextUpWithMultipleDelaying ^
      "  if two are delaying but one is dead just show living" ! testStartNextUpWithMultipleDelayingWithDead ^
      end

  private val startNextCommandStream = AutomationCommandSource.startNextCommandStream

  private def testEndDeadAndDelaying = {
    val state = getInitialState.transitionWith(List(
      killEvent(comb1), updateInitiativeEvent(io1_0, it => it.copy(state = InitiativeState.Delaying))))

    startNextCommandStream.get(state) must_== Some(EndRoundCommand(io1_0), startNextCommandStream)
  }

  private def testStartDeadAndReady = {
    val state = getInitialState.transitionWith(List(
      killEvent(comb1), updateInitiativeEvent(io1_0, it => it.copy(state = InitiativeState.Ready))))

    startNextCommandStream.get(state) must_== Some(StartRoundCommand(io1_0), startNextCommandStream)
  }

  private def testStartDeadAndWaiting = {
    val state = getInitialState.transitionWith(List(killEvent(comb1)))

    startNextCommandStream.get(state) must_== Some(StartRoundCommand(io1_0), startNextCommandStream)
  }

  private def testEndDeadAndActing = {
    val state = getInitialState.transitionWith(List(
      killEvent(comb1), updateInitiativeEvent(io1_0, it => it.copy(state = InitiativeState.Acting))))

    startNextCommandStream.get(state) must_== Some(EndRoundCommand(io1_0), startNextCommandStream)
  }


  private def testStartWaitingLiving = {
    val state = getInitialState

    startNextCommandStream.get(state) must_== Some(StartRoundCommand(io1_0), startNextCommandStream)
  }

  private def testStartReadyLiving = {
    val state = getInitialState.transitionWith(List(
      updateInitiativeEvent(io1_0, it => it.copy(state = InitiativeState.Ready))))

    startNextCommandStream.get(state) must_== Some(StartRoundCommand(io1_0), startNextCommandStream)
  }

  private def testEndRoundDelayingLiving = {
    val state = getInitialState.transitionWith(List(
      updateInitiativeEvent(io1_0, it => it.copy(state = InitiativeState.Delaying))))

    startNextCommandStream.get(state) must_== Some(EndRoundCommand(io1_0), startNextCommandStream)
  }


  def testDelayingLivingAutomation = {
    val state = getInitialState.transitionWith(List(
      updateInitiativeEvent(io1_0, it => it.copy(state = InitiativeState.Delaying))))

    val (nState, transitions) = miniDispatcher(startNextCommandStream, state)

    (nState.order.nextUp.get must_== io1_0) and (transitions must_== List(
      EndRoundCommand(io1_0),
      StartRoundCommand(io1_0)))
  }

  private def runCommands(iState: CombatState, cmds: CombatStateCommand*): CombatState = {
    cmds.foldLeft(iState)((state, cmd) => state.transitionWith(cmd.generateEvents(state)))
  }

  private def testStartNextUpWithOneDelaying = {
    val state = runCommands(getInitialState,
      StartRoundCommand(io1_0),
      DelayCommand(io1_0))
    startNextCommandStream.get(state) must_== Some(NextUpCommand(io2_0, List(io1_0)), startNextCommandStream)
  }

  private def testStartNextUpWithOneDelayingButDead = {
    val state = runCommands(getInitialState,
      StartRoundCommand(io1_0),
      DelayCommand(io1_0),
      DamageCommand(comb1, 1000))
    startNextCommandStream.get(state) must_== Some(StartRoundCommand(io2_0), startNextCommandStream)
  }

  private def testStartNextUpWithMultipleDelaying = {
    val state = runCommands(getInitialState,
      StartRoundCommand(io1_0),
      DelayCommand(io1_0),
      StartRoundCommand(io2_0),
      DelayCommand(io2_0))
    startNextCommandStream.get(state) must_== Some(NextUpCommand(ioB0, List(io1_0, io2_0)), startNextCommandStream)
  }

  private def testStartNextUpWithMultipleDelayingWithDead = {
    val state = runCommands(getInitialState,
      StartRoundCommand(io1_0),
      DelayCommand(io1_0),
      StartRoundCommand(io2_0),
      DelayCommand(io2_0),
      DamageCommand(comb2, 1000))
    startNextCommandStream.get(state) must_== Some(NextUpCommand(ioB0, List(io1_0)), startNextCommandStream)
  }

  def miniDispatcher(cs: CommandStream[CombatState], state: CombatState): (CombatState, List[Command[CombatState]]) = {
    var trans = List.empty[Command[CombatState]]
    var step = cs.get(state)
    var nState = state
    while (step.isDefined) {
      nState = nState.transitionWith(step.get._1.generateEvents(nState))
      trans = step.get._1 :: trans
      step = (step.get._2).get(nState)
    }
    (nState, trans.reverse)
  }

  private def testFullDeadAndStartNextAutomation = {
    val state = getInitialState.transitionWith(List(
      killEvent(comb1), killEvent(combB), killEvent(comb2),
      updateInitiativeEvent(io2_0, _.copy(state = InitiativeState.Delaying)),
      updateInitiativeEvent(io1_0, _.copy(state = InitiativeState.Ready))))

    val (nState, transitions) = miniDispatcher(startNextCommandStream, state)

    (nState.order.nextUp.get must_== ioA0) and (transitions must_== List(
      StartRoundCommand(io1_0), EndRoundCommand(io1_0),
      EndRoundCommand(io2_0), StartRoundCommand(io2_0), EndRoundCommand(io2_0),
      StartRoundCommand(ioB0), EndRoundCommand(ioB0),
      StartRoundCommand(ioA0)))
  }

  private def getInitialState: CombatState = {
    CombatState.empty.transitionWith(List(
      evtAddMonsterNoId, evtAddCombA, evtAddMonsterNoId, AddCombatantEvent(Some(combB), null, entityMonster),
      meAddToOrder(comb2, 1, 15), meAddToOrder(combB, 1, 14), meAddToOrder(comb1, 1, 20), meAddToOrder(combA, 2, 10),
      evtStart))
  }

  private def updateInitiativeEvent(ioi: InitiativeOrderID, mod: InitiativeTracker => InitiativeTracker): CombatStateEvent = {
    ForceChangeEvent(state => state.lensFactory.initiativeTrackerLens(ioi).mod(state, mod))
  }

  case class extractor() extends Mockito {
    val state0 = CombatState.empty.transitionWith(List(
      evtAddCombNoId, evtAddCombNoId, meAddToOrder(comb1, 0, 10), meAddToOrder(comb2, 0, 5), evtStart))

    def getsHeadHealthAndInitiativeTrackerWhenCombatStarted = {
      val (state1, mi) = mockCombatantAspect(state0, state0.lensFactory.initiativeTrackerLens(io1_0))
      val (state, mh) = mockCombatantAspect(state1, state1.lensFactory.combatantHealth(comb1))
      mi.state returns InitiativeState.Delaying
      mh.status returns HealthStatus.Bloody

      (AutomationCommandSource.HeadStateAndHealth.unapply(state) must_== Some((io1_0, InitiativeState.Delaying, HealthStatus.Bloody))) and
        (there was one(mi).state) and (there was one(mh).status)
    }

    def doesNotMatchOnceCombatEnded = {
      val state = state0.endCombat()
      AutomationCommandSource.HeadStateAndHealth.unapply(state) must_== None
    }

    private def mockCombatantAspect[T](state: CombatState, lens: Lens[CombatState, T])(implicit cm: ClassManifest[T]): (CombatState, T) = {
      val mockT = mock[T](cm)
      (lens.mod(state, x => mockT), mockT)
    }
  }
}