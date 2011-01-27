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

import vcc.controller.{Decision, Ruling}
import vcc.infra.prompter.ValuePanel.Return
import vcc.dnd4e.domain.tracker.common.{SaveEffectDecision, SaveEffectRuling}
import vcc.infra.prompter.{RadioButtonValuePanel, ValuePanel, RulingPromptController, RulingTranslatorService}

/**
 * TranslatorService companion object
 */
object TranslatorService {
  def getInstance(): RulingTranslatorService = new TranslatorService()
}

/**
 * Implementation of RulingTranslatorService that handle DND4E tracker Ruling RulingPromptController wrapper.
 */
class TranslatorService extends RulingTranslatorService {
  def promptForRuling[R <: Ruling](ruling: Ruling): RulingPromptController[R] = {
    ruling match {
    //TODO all the cases
      case r@SaveEffectRuling(eid, effect) => new SimpleSavePromptController(r).asInstanceOf[RulingPromptController[R]]
      case leftover => throw new Exception("No translation for: " + leftover)
    }
  }
}

class SimpleSavePromptController(ruling: SaveEffectRuling) extends RulingPromptController[SaveEffectRuling] {
  private var decision: SaveEffectDecision = null

  def extractDecision(): Decision[SaveEffectRuling] = decision

  def hasAnswer: Boolean = decision != null

  def handleAccept(value: Return): Boolean = {
    value match {
      case RadioButtonValuePanel.Return(r) =>
        decision = r match {
          case Some(0) => SaveEffectDecision(ruling, true)
          case Some(1) => SaveEffectDecision(ruling, false)
          case _ => null
        }
        decision != null
      case _ => false
    }
  }

  def decoratePanel(panel: ValuePanel[_]) {
    val radioPanel = panel.asInstanceOf[RadioButtonValuePanel]
    decision match {
      case SaveEffectDecision(_, state) => radioPanel.setValue(Option(if (state) 0 else 1))
      case _ => panel.setValue(None)
    }
  }

  def panelIdentity(): String = RulingDialog.SimpleSavePanelIdentity

  def prompt(): String = "Save: " + ruling.text
}

