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

import vcc.controller.Decision
import vcc.infra.prompter.ValuePanel.Return
import vcc.infra.prompter.{ValuePanel, RulingPromptController}
import vcc.dnd4e.domain.tracker.common.{SaveEffectSpecialDecision, SaveEffectSpecialRuling}

class SaveSpecialPromptController(ruling: SaveEffectSpecialRuling) extends RulingPromptController[SaveEffectSpecialRuling] {

  import vcc.dnd4e.domain.tracker.common.SaveEffectSpecialDecision._

  private var result: Option[SaveEffectSpecialDecision.Result] = None

  def extractDecision(): Decision[SaveEffectSpecialRuling] = result match {
    case Some(Saved) => SaveEffectSpecialDecision(ruling, SaveEffectSpecialDecision.Saved)
    case Some(Changed(c)) => SaveEffectSpecialDecision(ruling, SaveEffectSpecialDecision.Changed(c))
    case None => null
  }

  def hasAnswer: Boolean = result.isDefined

  def handleAccept(value: Return): Boolean = {
    value match {
      case SaveOrChangeValuePanel.Value(v) if (v.isDefined) =>
        this.result = v
        true
      case _ => false
    }
  }

  def decoratePanel(panel: ValuePanel[_]) {
    val scPanel = panel.asInstanceOf[ValuePanel[SaveEffectSpecialDecision.Result]]
    scPanel.setValue(result)
    result match {
      case Some(Changed(v)) => scPanel.setField("NewCondition", v)
      case _ => scPanel.setField("NewCondition", progressCondition(ruling.condition))
    }
  }

  def panelIdentity(): String = SaveOrChangeValuePanel.Identity

  def prompt(): String = "Save: " + ruling.condition

  private[ruling] def progressCondition(oldCondition: String): String = {
    val sp = oldCondition.split("""\s*(\->|\/)\s*""") //.map(_.trim)
    if (sp.length > 1) sp.tail.mkString(" -> ")
    else sp(0)
  }
}