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
package vcc.dnd4e.tracker.dispatcher

import vcc.dnd4e.tracker.common.CombatState
import vcc.controller.message.TransactionalAction
import vcc.tracker._
import org.slf4j.Logger

class Dispatcher private(log: Logger) {

  private val translator: ActionStreamTranslator[CombatState, TransactionalAction] = ActionTranslator

  private val commandDispatcher: StateCommandDispatcher[CombatState] = new StateCommandDispatcher[CombatState](
    new RulingDispatcher[CombatState](
      new RulingPeer[CombatState] {
        def provideDecisionForRuling(state: CombatState, rulings: List[Ruling[CombatState, _, _, _]]): List[Ruling[CombatState, _, _, _]] = {
          Nil
        }
      },
      new RulingLocationService[CombatState] {
        def rulingsFromStateWithCommand(state: CombatState, command: StateCommand[CombatState]): List[Ruling[CombatState, _, _, _]] = {
          Nil
        }
      }
    ))

  private def regularDispatch(state: CombatState, action: TransactionalAction): Transaction[CombatState, TransactionalAction] = {
    val builder = new AccumulatorTransitionBuilder[CombatState, TransactionalAction]()
    val executor = new ActionExecutor(translator, commandDispatcher, builder)
    executor.executeCommand(state, action)
  }

  private def debugDispatch(state: CombatState, action: TransactionalAction): Transaction[CombatState, TransactionalAction] = {
    val builder = new DebugTransitionBuilder(new AccumulatorTransitionBuilder[CombatState, TransactionalAction], log)
    val debugExecutor = new ActionExecutor(translator, commandDispatcher, builder)
    debugExecutor.executeCommand(state, action)
  }

  def dispatch(state: CombatState, action: TransactionalAction): Transaction[CombatState, TransactionalAction] = {
    try {
      regularDispatch(state, action)
    } catch {
      case e =>
        debugDispatch(state, action)
    }
  }
}

object Dispatcher {
  def getInstance(log: Logger): Dispatcher = {
    new Dispatcher(log)
  }
}