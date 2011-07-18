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
package vcc.dnd4e.view.ruling

import vcc.controller.Decision
import vcc.infra.prompter.ValuePanel.Return
import vcc.infra.prompter.{EnumerationValuePanel, ValuePanel, RulingPromptController}
import vcc.dnd4e.domain.tracker.common.{SaveVersusDeathDecision, SaveVersusDeathRuling}

class SaveVersusDeathPromptController(ruling: SaveVersusDeathRuling) extends RulingPromptController[SaveVersusDeathRuling] {

  import SaveVersusDeathDecision._

  private var decision: SaveVersusDeathDecision = null

  def extractDecision(): Decision[SaveVersusDeathRuling] = decision

  def hasAnswer: Boolean = decision != null

  def handleAccept(value: Return): Boolean = {
    value match {
      case EnumerationValuePanel.Value(v) =>
        decision = v match {
          case Some(Saved) => SaveVersusDeathDecision(ruling, Saved)
          case Some(Failed) => SaveVersusDeathDecision(ruling, Failed)
          case Some(SaveAndHeal) => SaveVersusDeathDecision(ruling, SaveAndHeal)
          case _ => null
        }
        decision != null
      case _ => false
    }
  }

  def decoratePanel(panel: ValuePanel[_]) = {
    val enumPanel = panel.asInstanceOf[EnumerationValuePanel[SaveVersusDeathDecision.type]]
    if (decision != null) enumPanel.setValue(Some(decision.result))
    else enumPanel.setValue(None)
  }

  def panelIdentity(): String = RulingDialog.SaveVersusDeathPanelIdentity

  def prompt(): String = "Saving throw versus death"
}