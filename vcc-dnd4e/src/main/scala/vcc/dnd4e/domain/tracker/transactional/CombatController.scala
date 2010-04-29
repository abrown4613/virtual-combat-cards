/**
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
import scala.collection.mutable.Queue
import vcc.dnd4e.domain.tracker.common.{CombatStateChange, CombatStateRules}
import vcc.controller.transaction.ChangeNotification
import vcc.controller.message.{TrackerChanged, TransactionalAction}

/**
 * This Mixin implement the <code>publish</code> method required for a AbstractTrackerController, it will
 * simply cast all objects to the CombatStateChange and make sure every changes was accounted for.
 */
trait TrackerControllerValidatingPublisher {
  def publish(changes: Seq[ChangeNotification]): Any = {
    val c: Seq[CombatStateChange] = for (change <- changes if (change.isInstanceOf[CombatStateChange])) yield {
      change.asInstanceOf[CombatStateChange]
    }
    assert(c.length == changes.length)
    TrackerChanged(c.toList.toList)
  }

}


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
                with EffectActionHandler
                with HealthActionHandler
{
  def this(rules: CombatStateRules, state: CombatState) = this (rules, state, new Queue[TransactionalAction]())
}
