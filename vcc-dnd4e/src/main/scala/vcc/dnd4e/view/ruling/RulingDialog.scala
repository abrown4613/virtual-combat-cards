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
import vcc.util.swing.{MigPanel, ModalPromptDialog}
import vcc.tracker.{RulingContext, Ruling}
import vcc.dnd4e.tracker.command.{EndRoundCommand, StartRoundCommand}
import vcc.dnd4e.tracker.common.{InitiativeOrderID, CombatState}
import vcc.dnd4e.tracker.ruling.SustainEffectRuling
import org.exnebula.swing.{PromptPanel, JPromptPanelList}
import org.exnebula.swing.PromptPanel.EditCompletionListener
import swing.{RadioButton, Component, Button}
import javax.swing.{JLabel, JComponent}
import java.lang.String

object RulingDialog {

  private class SustainRulingPanel extends PromptPanel {
    private val editor = new MigPanel("") {
      add(new RadioButton("Sustain"))
      add(new RadioButton("Cancel"))
    }

    def setEditCompletionListener(p1: EditCompletionListener) {}

    def getViewComponent: JComponent = new JLabel()

    def getEditorComponent: JComponent = editor.peer

    def adjustFocusToEditor() {}

    def getPromptTile: String = ""
  }

}

class RulingDialog(context: RulingContext[CombatState], owner: Window)
  extends ModalPromptDialog[List[Ruling[CombatState, _, _]]](owner, "") {

  import RulingDialog._

  private val promptPanel = new JPromptPanelList()

  promptPanel.setRowHeight(50)

  contents = new MigPanel("debug") {
    add(new Component {
      override lazy val peer = promptPanel
    }, "span 3,width 300, height 150, wrap")
    add(new Button(okAction))
    add(new Button(cancelAction))
  }

  peer.setTitle(translateCommandToTitle())
  if (!context.rulingNeedingDecision.isEmpty) {
    val panel = context.rulingNeedingDecision(0) match {
      case SustainEffectRuling(eid, _) => new SustainRulingPanel()
    }
    promptPanel.addPromptPanel(panel)
    promptPanel.setActivePrompt(0)
    okAction.enabled = false
  }

  def collectResult(): Option[List[Ruling[CombatState, _, _]]] = Option(context.rulingNeedingDecision)

  private def translateCommandToTitle(): String = {
    context.triggeringCommand match {
      case StartRoundCommand(ioi) => makeFormattedCombatantName(ioi) + " - Start Round"
      case EndRoundCommand(ioi) => makeFormattedCombatantName(ioi) + " - End Round"
      case _ => "Unknown event"
    }
  }

  private def makeFormattedCombatantName(ioi: InitiativeOrderID): String = {
    "[%s] %s".format(ioi.toLabelString, context.state.combatant(ioi.combId).name)
  }

}