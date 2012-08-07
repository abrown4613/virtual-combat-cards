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
package vcc.dnd4e.view

import ruling.{NextUpRulingDialog, RulingPrompt}
import vcc.dnd4e.tracker.common.CombatState
import vcc.tracker.{Ruling, RulingContext, RulingProvider}
import vcc.dnd4e.tracker.ruling.NextUpRuling
import vcc.dnd4e.tracker.command.NextUpCommand

class DialogRulingProvider extends  RulingProvider[CombatState] {

    def provideRulingFor(context: RulingContext[CombatState]): List[Ruling[CombatState, _, _]] = {
      context.rulingNeedingDecision match {
        case nur@NextUpRuling(next, _) :: Nil => askForNextUp(context.state, next)
        case other =>
          RulingPrompt.promptUser(context)
      }
    }

    private def askForNextUp(state: CombatState, next: NextUpCommand): scala.List[NextUpRuling] = {
      val dialog = new NextUpRulingDialog(null, state, NextUpRuling(next, None))
      val result = dialog.promptUser()
      dialog.dispose()
      result.toList
    }
}