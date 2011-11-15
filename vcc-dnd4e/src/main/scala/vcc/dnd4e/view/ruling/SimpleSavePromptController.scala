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

import vcc.infra.prompter.ValuePanel.Return
import vcc.controller.Decision
import vcc.dnd4e.domain.tracker.common.{SaveEffectDecision, SaveEffectRuling}
import vcc.infra.prompter.{EnumerationValuePanel, ValuePanel, RulingPromptController}

class SimpleSavePromptController(ruling: SaveEffectRuling) extends RulingPromptController[SaveEffectRuling] {
  private var decision: SaveEffectDecision = null

  def extractDecision(): Decision[SaveEffectRuling] = decision

  def hasAnswer: Boolean = decision != null

  def handleAccept(value: Return): Boolean = {
    value match {
      case EnumerationValuePanel.Value(r) =>
        decision = r match {
          case Some(SaveEffectDecision.Saved) => SaveEffectDecision(ruling, SaveEffectDecision.Saved)
          case Some(SaveEffectDecision.Failed) => SaveEffectDecision(ruling, SaveEffectDecision.Failed)
          case _ => null
        }
        decision != null
      case _ => false
    }
  }

  def decoratePanel(panel: ValuePanel[_]) {
    val radioPanel = panel.asInstanceOf[EnumerationValuePanel[SaveEffectDecision.type]]
    decision match {
      case SaveEffectDecision(_, state) => radioPanel.setValue(Some(state))
      case _ => panel.setValue(None)
    }
  }

  def panelIdentity(): String = RulingDialog.SimpleSavePanelIdentity

  def prompt(): String = "Save: " + ruling.condition
}
