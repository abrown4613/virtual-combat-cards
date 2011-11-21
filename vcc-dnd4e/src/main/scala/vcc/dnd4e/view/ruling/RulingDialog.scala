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
package vcc.dnd4e.view.ruling

import java.awt.Window
import vcc.dnd4e.tracker.common.CombatState
import vcc.util.swing.{MigPanel, ModalPromptDialog}
import swing.Button
import vcc.tracker.{RulingContext, Ruling}

class RulingDialog(context: RulingContext[CombatState], owner: Window)
  extends ModalPromptDialog[List[Ruling[CombatState, _, _]]](owner, "") {

  println("Context: " + context)

  contents = new MigPanel("debug") {
    add(new Button(okAction))
    add(new Button(cancelAction))
  }

  def collectResult(): Option[List[Ruling[CombatState, _, _]]] = Option(context.rulingNeedingDecision)
}