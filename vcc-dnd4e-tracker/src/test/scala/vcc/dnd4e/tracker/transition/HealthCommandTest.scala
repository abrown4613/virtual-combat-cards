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
import vcc.controller.IllegalActionException
import vcc.dnd4e.tracker.event._

class HealthCommandTest extends SpecificationWithJUnit with CombatStateEventSourceBehavior with EventSourceSampleEvents {
  def is = {
    "Health change command".title ^
      damaging ^
      runTestFor(cid => (HealCommand(cid, 10), ApplyHealingEvent(cid, 10))) ^
      runTestFor(cid => (SetTemporaryHPCommand(cid, 10), SetTemporaryHitPointsEvent(cid, 10))) ^
      runTestFor(cid => (FailDeathSaveCommand(cid), FailDeathSaveEvent(cid))) ^
      runTestFor(cid => (RevertDeathCommand(cid), RevertDeathEvent(cid))) ^
      end
  }

  def runTestFor(builder: CombatantID => (CombatStateCommand, CombatStateEvent)) = {
    val (cmd, evt) = builder(combA)
    ("Command " + cmd.getClass.getSimpleName) ^
      "fail if not found" !
        (given(emptyState) when (cmd) failWith notFoundException) ^
      "generate event" !
        (given(emptyState, evtAddCombA) when (cmd) then (evt)) ^
      endp

  }

  private val notFoundException = new IllegalActionException("Combatant " + combA + " not in combat")

  private def damaging = {
    "command ApplyDamageTransition" ^
      "fail if combantant is not present" !
        (given(emptyState, evtAddCombNoId).
          when(DamageCommand(combA, 10)).
          failWith(notFoundException)) ^
      "simple damage" !
        (given(emptyState, evtAddCombA, evtAddCombNoId, evtAddCombNoId, evtInitA, meAddToOrder(comb1, 3, 3), evtStart).
          when(DamageCommand(combA, 10)).
          then(ApplyDamageEvent(combA, 10))) ^
      "damage and end combat when last acting is killed" !
        (given(emptyState, evtAddCombA, evtAddCombNoId, evtAddCombNoId, evtInitA,
          meAddToOrder(comb1, 3, 3), evtStart, killEvent(comb1)).
          when(DamageCommand(combA, 1000)).
          then(ApplyDamageEvent(combA, 1000), evtEnd)) ^
      "killing but not the last does kill" !
        (given(emptyState, evtAddCombA, evtAddCombNoId, evtAddCombNoId, evtInitA,
          meAddToOrder(comb1, 3, 3), evtStart).
          when(DamageCommand(combA, 1000)).
          then(ApplyDamageEvent(combA, 1000))) ^
      endp
  }
}