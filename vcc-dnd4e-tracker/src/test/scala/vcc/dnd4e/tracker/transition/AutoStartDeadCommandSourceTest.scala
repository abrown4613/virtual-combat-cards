package vcc.dnd4e.tracker.transition

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
//$Id$

import org.specs2.SpecificationWithJUnit
import vcc.dnd4e.tracker.StateLensFactory
import org.specs2.mock.Mockito
import vcc.tracker.CommandStream
import vcc.dnd4e.tracker.common._

class AutoStartDeadCommandSourceTest extends SpecificationWithJUnit with SampleStateData {

  def is =
    "HeadStateAndHealth" ^
      "  extract proper entry on started combat" ! Abc().e1 ^
      "  extract None if combat not started" ! Abc().e2 ^
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

  def testEndDeadAndDelaying = {
    val state = getStateBuilder.
      modifyHealth(comb1, StateBuilder.kill).
      modifyInitiative(io1_0, it => it.copy(state = InitiativeTracker.state.Delaying)).
      done

    cs.get(state) must_== Some(EndRoundTransition(io1_0), cs)
  }

  def testStartDeadAndReady = {
    val state = getStateBuilder.
      modifyHealth(comb1, StateBuilder.kill).
      modifyInitiative(io1_0, it => it.copy(state = InitiativeTracker.state.Ready)).
      done

    cs.get(state) must_== Some(StartRoundTransition(io1_0), cs)
  }

  def testStartDeadAndWaiting = {
    val state = getStateBuilder.
      modifyHealth(comb1, StateBuilder.kill).
      done

    cs.get(state) must_== Some(StartRoundTransition(io1_0), cs)
  }

  def testEndDeadAndActing = {
    val state = getStateBuilder.
      modifyHealth(comb1, StateBuilder.kill).
      modifyInitiative(io1_0, it => it.copy(state = InitiativeTracker.state.Acting)).
      done

    cs.get(state) must_== Some(EndRoundTransition(io1_0), cs)
  }


  def testStartWaitingLiving = {
    val state = getStateBuilder.done

    ns.get(state) must_== Some(StartRoundTransition(io1_0), ns)
  }

  def testStartReadyLiving = {
    val state = getStateBuilder.
      modifyInitiative(io1_0, it => it.copy(state = InitiativeTracker.state.Ready)).
      done

    ns.get(state) must_== Some(StartRoundTransition(io1_0), ns)
  }

  def testEndRoundDelayingLiving = {
    val state = getStateBuilder.
      modifyInitiative(io1_0, it => it.copy(state = InitiativeTracker.state.Delaying)).
      done

    ns.get(state) must_== Some(EndRoundTransition(io1_0), ns)
  }


  def testDelayingLivingAutomation = {
    val state = workAroundToNoSymbol.
      modifyInitiative(io1_0, it => it.copy(state = InitiativeTracker.state.Delaying)).
      done

    val (nState, transitions) = miniDispatcher(ns, state)

    (nState.order.nextUp.get must_== io1_0) and (transitions must_== List(
      EndRoundTransition(io1_0),
      StartRoundTransition(io1_0)))
  }


  //This is a workaround to a no-symbol error on the compiler
  private def workAroundToNoSymbol = getStateBuilder.modifyInitiative(io1_0, it => it.copy(state = InitiativeTracker.state.Ready))


  def miniDispatcher(cs: CommandStream[CombatState, CombatTransition], state: CombatState): (CombatState, List[CombatTransition]) = {
    var trans = List.empty[CombatTransition]
    var step = cs.get(state)
    var nState = state
    while (step.isDefined) {
      nState = step.get._1.transition(StateLensFactory, nState)
      trans = step.get._1 :: trans
      step = (step.get._2).get(nState)
    }
    (nState, trans.reverse)
  }

  def testFullDeadAutomation = {
    val state = workAroundToNoSymbol.
      modifyHealth(comb1, StateBuilder.kill).
      modifyHealth(combB, StateBuilder.kill).
      modifyHealth(comb2, StateBuilder.kill).
      modifyInitiative(io2_0, it => it.copy(state = InitiativeTracker.state.Delaying)).
      done

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

  def getStateBuilder = {
    StateBuilder.emptyState().
      addCombatant(Some(comb1), null, entityMonster).
      addCombatant(Some(combA), null, entityPc1).
      addCombatant(Some(comb2), null, entityMonster).
      addCombatant(Some(combB), null, entityMonster).
      setInitiative(comb2, 15).
      setInitiative(combB, 14).
      setInitiative(comb1, 20).
      setInitiative(combA, 10).
      startCombat()
  }

  case class Abc() extends Mockito {
    val mh = mock[HealthTracker]
    val mi = mock[InitiativeTracker]
    mh.status() returns HealthTracker.Status.Bloody
    mi.state returns InitiativeTracker.state.Delaying
    val state = StateBuilder.emptyState().
      addCombatant(None, null, entityMonster).
      addCombatant(None, null, entityMonster).
      setInitiative(comb1, 10).
      setInitiative(comb2, 5).
      startCombat().
      modifyHealth(comb1, x => mh).
      modifyInitiative(io1_0, x => mi).
      done

    def e1 = {
      (AutomationCommandSource.HeadStateAndHealth.unapply(state) must_== Some((io1_0, InitiativeTracker.state.Delaying, HealthTracker.Status.Bloody))) and
        (there was one(mi).state) and (there was one(mh).status())
    }

    def e2 = AutomationCommandSource.HeadStateAndHealth.unapply(state.endCombat()) must_== None
  }

}
