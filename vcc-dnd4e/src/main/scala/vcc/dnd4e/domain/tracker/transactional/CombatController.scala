/**
 * Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.transactional

import vcc.controller.TransactionalProcessor
import vcc.dnd4e.domain.tracker.common.CombatStateRules
import vcc.dnd4e.controller.TrackerControllerValidatingPublisher
import scala.collection.mutable.Queue
import vcc.controller.message.TransactionalAction

/**
 * Base class for testing and build CombatController, this is used for isolated testing of individual handlers.
 */
abstract class AbstractCombatController(val rules: CombatStateRules, val state: CombatState, queue: Queue[TransactionalAction])
        extends TransactionalProcessor(state, queue)
                with TrackerControllerValidatingPublisher

/**
 * This is the final combat controller that includes all action handlers
 */
class CombatController(rules: CombatStateRules, state: CombatState, queue: Queue[TransactionalAction])
        extends AbstractCombatController(rules, state, queue)
                with CombatStateActionHandler
                with InitiativeActionHandler
{
  def this(rules: CombatStateRules, state: CombatState) = this (rules, state, new Queue[TransactionalAction]())
}
