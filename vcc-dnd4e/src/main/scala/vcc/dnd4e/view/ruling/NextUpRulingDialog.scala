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

import vcc.dnd4e.tracker.ruling.NextUpRuling
import java.awt.Window
import vcc.util.swing.{MigPanel, ModalPromptDialog}
import vcc.dnd4e.tracker.common.{CombatantID, CombatState}
import swing.{ScrollPane, ListView, Button}

class NextUpRulingDialog(owner: Window, state: CombatState, nextUpRuling: NextUpRuling)
  extends ModalPromptDialog[NextUpRuling](owner, "Select next combatant to act") {

  private val orderIds = Seq(nextUpRuling.candidates.next) ++ nextUpRuling.candidates.delaying
  private val optionList = initializeCombatantList()
  private val okButton = new Button(okAction)

  contents = new MigPanel("fill", "[300]", "[80][]") {
    add(new ScrollPane(optionList), "growx, growy, wrap")
    add(okButton, "split 3")
    add(new Button(cancelAction), "")
  }
  okButton.requestFocus()
  peer.getRootPane.setDefaultButton(okButton.peer)
  this.peer.pack()
  this.placeOnScreenCenter()

  def collectResult(): Option[NextUpRuling] = {
    Some(nextUpRuling.withDecision(orderIds(optionList.selection.leadIndex)))
  }

  private def initializeCombatantList() = {
    val listView = new ListView[String](
      Seq(formatCombatantName(nextUpRuling.candidates.next.combId, "Next in order")) ++
        nextUpRuling.candidates.delaying.map(orderId => formatCombatantName(orderId.combId, "Delaying")))
    listView.selectIndices(0)
    listView.selection.intervalMode = ListView.IntervalMode.Single
    listView
  }

  private def formatCombatantName(cid: CombatantID, suffix: String): String = {
    " %s %s - %s ".format(cid.simpleNotation, state.combatant(cid).name, suffix)
  }

}