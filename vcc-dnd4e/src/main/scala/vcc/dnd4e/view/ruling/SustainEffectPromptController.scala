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
import vcc.dnd4e.domain.tracker.common.{SustainEffectDecision, SustainEffectRuling}


class SustainEffectPromptController(ruling: SustainEffectRuling) extends RulingPromptController[SustainEffectRuling] {

  import SustainEffectDecision._

  private var decision: SustainEffectDecision = null

  def extractDecision(): Decision[SustainEffectRuling] = decision

  def hasAnswer: Boolean = decision != null

  def handleAccept(value: Return): Boolean = {
    value match {
      case EnumerationValuePanel.Value(v) =>
        decision = v match {
          case Some(Sustain) => SustainEffectDecision(ruling, Sustain)
          case Some(Cancel) => SustainEffectDecision(ruling, Cancel)
          case _ => null
        }
        decision != null
      case _ => false
    }
  }

  def decoratePanel(panel: ValuePanel[_]) = {
    val enumPanel = panel.asInstanceOf[EnumerationValuePanel[SustainEffectDecision.type]]
    if (decision != null) enumPanel.setValue(Some(decision.result))
    else enumPanel.setValue(None)
  }

  def panelIdentity(): String = RulingDialog.SustainEffectPanelIdentity

  def prompt(): String = "Sustain: " + ruling.condition
}