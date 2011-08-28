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
import vcc.tracker.{StateCommand, CommandStream}

class AutoStartDeadCommandSourceTest extends SpecificationWithJUnit with EventSourceSampleEvents {

  def is =
    "HeadStateAndHealth" ^
      "  extract proper entry on started combat" ! extractor().e1 ^
      "  extract None if combat not started" ! extractor().e2 ^
      endp ^
      "Automatic dead handling" ^
      "  auto end round of dead delaying" ! testEndDeadAndDelaying ^
      "  auto start round of dead waiting" ! testStartDeadAndWaiting ^
      "  auto start round of dead ready" ! testStartDeadAndReady ^
      "  auto end round of dead acting" ! testEndDeadAndActing ^
      "  all together" ! testFullDeadAutomation ^
      endp ^
      "Automatic start living" ^
      "  auto start first Waiting" ! testStartWaitingLiving ^
      "  auto start first Ready" ! testStartReadyLiving ^
      "  auto end first Delaying living" ! testEndRoundDelayingLiving ^
      "  on living delay must end and start round" ! testDelayingLivingAutomation ^
      end

  private val cs = AutomationCommandSource.autoStartDead
  private val ns = AutomationCommandSource.autoStartNext

  private def testEndDeadAndDelaying = {
    val state = getInitialState.transitionWith(List(
      killEvent(comb1), X.initiativeChanger(io1_0, it => it.copy(state = InitiativeState.Delaying))))

    cs.get(state) must_== Some(EndRoundTransition(io1_0), cs)
  }

  private def testStartDeadAndReady = {
    val state = getInitialState.transitionWith(List(
      killEvent(comb1), X.initiativeChanger(io1_0, it => it.copy(state = InitiativeState.Ready))))

    cs.get(state) must_== Some(StartRoundTransition(io1_0), cs)
  }

  private def testStartDeadAndWaiting = {
    val state = getInitialState.transitionWith(List(killEvent(comb1)))

    cs.get(state) must_== Some(StartRoundTransition(io1_0), cs)
  }

  private def testEndDeadAndActing = {
    val state = getInitialState.transitionWith(List(
      killEvent(comb1), X.initiativeChanger(io1_0, it => it.copy(state = InitiativeState.Acting))))

    cs.get(state) must_== Some(EndRoundTransition(io1_0), cs)
  }


  private def testStartWaitingLiving = {
    val state = getInitialState

    ns.get(state) must_== Some(StartRoundTransition(io1_0), ns)
  }

  private def testStartReadyLiving = {
    val state = getInitialState.transitionWith(List(
      X.initiativeChanger(io1_0, it => it.copy(state = InitiativeState.Ready))))

    ns.get(state) must_== Some(StartRoundTransition(io1_0), ns)
  }

  private def testEndRoundDelayingLiving = {
    val state = getInitialState.transitionWith(List(
      X.initiativeChanger(io1_0, it => it.copy(state = InitiativeState.Delaying))))

    ns.get(state) must_== Some(EndRoundTransition(io1_0), ns)
  }


  def testDelayingLivingAutomation = {
    val state = getInitialState.transitionWith(List(
      X.initiativeChanger(io1_0, it => it.copy(state = InitiativeState.Delaying))))

    val (nState, transitions) = miniDispatcher(ns, state)

    (nState.order.nextUp.get must_== io1_0) and (transitions must_== List(
      EndRoundTransition(io1_0),
      StartRoundTransition(io1_0)))
  }

  def miniDispatcher(cs: CommandStream[CombatState, EventCombatTransition], state: CombatState): (CombatState, List[EventCombatTransition]) = {
    var trans = List.empty[EventCombatTransition]
    var step = cs.get(state)
    var nState = state
    while (step.isDefined) {
      nState = nState.transitionWith(step.get._1.changeEvents(nState))
      trans = step.get._1 :: trans
      step = (step.get._2).get(nState)
    }
    (nState, trans.reverse)
  }

  private def testFullDeadAutomation = {
    val state = getInitialState.transitionWith(List(
      killEvent(comb1), killEvent(combB), killEvent(comb2),
      X.initiativeChanger(io2_0, _.copy(state = InitiativeState.Delaying)),
      X.initiativeChanger(io1_0, _.copy(state = InitiativeState.Ready))))

    val (nState, transitions) = miniDispatcher(cs, state)

    (nState.order.nextUp.get must_== ioA0) and (transitions must_== List(
      StartRoundTransition(io1_0),
      EndRoundTransition(io1_0),
      EndRoundTransition(io2_0),
      StartRoundTransition(io2_0),
      EndRoundTransition(io2_0),
      StartRoundTransition(ioB0),
      EndRoundTransition(ioB0)))
  }

  private def getInitialState: CombatState = {
    CombatState.empty.transitionWith(List(
      evtAddMonsterNoId, evtAddCombA, evtAddMonsterNoId, AddCombatantEvent(Some(combB), null, entityMonster),
      meAddToOrder(comb2, 1, 15), meAddToOrder(combB, 1, 14), meAddToOrder(comb1, 1, 20), meAddToOrder(combA, 2, 10),
      evtStart))
  }

  object X extends Mockito {

    def mockCombatantAspect[T](state: CombatState, lens: Lens[CombatState, T])(implicit cm: ClassManifest[T]): (CombatState, T) = {
      val mockT = mock[T](cm) //(lens.get(state))
      (lens.mod(state, x => mockT), mockT)
    }

    def spyCombatantAspect[T](state: CombatState, lens: Lens[CombatState, T]): (CombatState, T) = {
      val spyT = spy(lens.get(state))
      (lens.mod(state, x => spyT), spyT)
    }

    /**
     * THis is hack to allow manipulating state and changing
     */
    def initiativeChanger(ioi: InitiativeOrderID, mod: InitiativeTracker => InitiativeTracker): CombatStateEvent = {
      ForceChangeEvent(state => state.lensFactory.initiativeTrackerLens(ioi).mod(state, mod))
    }
  }

  case class extractor() extends Mockito {
    val state0 = CombatState.empty.transitionWith(List(
      evtAddCombNoId, evtAddCombNoId, meAddToOrder(comb1, 0, 10), meAddToOrder(comb2, 0, 5), evtStart))

    def e1 = {
      val (state1, mi) = X.mockCombatantAspect(state0, state0.lensFactory.initiativeTrackerLens(io1_0))
      mi.state returns InitiativeState.Delaying
      val (state, mh) = X.mockCombatantAspect(state1, state1.lensFactory.combatantHealth(comb1))
      mh.status returns HealthStatus.Bloody

      (AutomationCommandSource.HeadStateAndHealth.unapply(state) must_== Some((io1_0, InitiativeState.Delaying, HealthStatus.Bloody))) and
        (there was one(mi).state) and (there was one(mh).status)
    }

    def e2 = {
      val state = state0.endCombat()
      AutomationCommandSource.HeadStateAndHealth.unapply(state) must_== None
    }
  }

}
