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
package vcc.dnd4e.tracker.dispatcher

import vcc.dnd4e.domain.tracker.transactional.AbstractCombatController
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.domain.tracker.common.Command._
import InitiativeTracker.action

trait MigrationHandler {
  this: AbstractCombatController =>

  addRewriteRule {
    case InitiativeAction(it, action.Delay) =>
      Seq(InternalInitiativeAction(it, action.StartRound), InternalInitiativeAction(it, action.Delay))

    case InitiativeAction(it, action.Ready) =>
      Seq(InternalInitiativeAction(it, action.Ready), InternalInitiativeAction(it, action.EndRound))

    case InitiativeAction(it, action) => Seq(InternalInitiativeAction(it, action))
  }


  addHandler {
    case action =>
      val ts = ActionTranslator.translate(action)
      println("Action: " + action + "Mapped to: " + ts.mkString(" + "))
      context.iState.value = context.iState.value.transitionWith(ts)
      println("New State: " + context.iState.value)
  }
}