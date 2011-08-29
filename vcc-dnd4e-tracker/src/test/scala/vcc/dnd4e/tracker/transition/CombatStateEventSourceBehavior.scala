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

import vcc.specs2.EventSourceBehavior
import org.specs2.Specification
import vcc.dnd4e.tracker.common.CombatState
import vcc.dnd4e.tracker.event.CombatStateEvent

trait CombatStateEventSourceBehavior extends EventSourceBehavior[CombatState, CombatStateEvent, CombatStateCommand] {
  self: Specification =>

  implicit val whenCommandRunner: (CombatState, CombatStateCommand) => Seq[CombatStateEvent] = {
    (s, c) => c.generateTransitions(s)
  }

  implicit val stateBuilderFromEvents: (CombatState, Seq[CombatStateEvent]) => CombatState = {
    (s, evts) => s.transitionWith(evts.toList)
  }

}