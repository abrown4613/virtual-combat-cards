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
import vcc.controller.{Decision, Ruling}
import vcc.infra.prompter.{TextFieldValuePanel, ValuePanel, RulingPromptController}
import vcc.dnd4e.domain.tracker.common.{RegenerateByDecision, RegenerateByRuling, OngoingDamageDecision, OngoingDamageRuling}

/**
 * Helper trait to with decoration of the DamageHealPromptController is delegated.
 */
trait DamageHealPromptControllerDelegate[R <: Ruling] {

  /**
   * Panel ID to be used of this delegate
   */
  val panelId: String

  /**
   *  Provide prompt from Ruling
   * @param ruling Ruling
   */
  def prompt(ruling: R): String

  /**
   *  Get the value read a provide a Decision for that value and ruling.
   * @param ruling Base ruling
   * @param value Value form the dialog
   */
  def buildDecision(ruling: R, value: Int): Decision[R]

  /**
   * Provides a hint of what initial value for the editField
   */
  def startValue(ruling: R): Option[Int]
}

object OngoingPromptControllerDelegate extends DamageHealPromptControllerDelegate[OngoingDamageRuling] {
  def startValue(ruling: OngoingDamageRuling): Option[Int] = Some(ruling.hintValue)

  def buildDecision(ruling: OngoingDamageRuling, value: Int): Decision[OngoingDamageRuling] = OngoingDamageDecision(ruling, value)

  def prompt(ruling: OngoingDamageRuling): String = ruling.description

  val panelId: String = "OngoingDamagePanel"
}

object RegeneratePromptControllerDelegate extends DamageHealPromptControllerDelegate[RegenerateByRuling] {
  def startValue(ruling: RegenerateByRuling): Option[Int] = Some(ruling.hintValue)

  def buildDecision(ruling: RegenerateByRuling, value: Int): Decision[RegenerateByRuling] = RegenerateByDecision(ruling, value)

  def prompt(ruling: RegenerateByRuling): String = ruling.description

  val panelId: String = "RegenerateDamagePanel"
}

/**
 * Controller for the DamageHealValuePanel.
 */
class DamageHealPromptController[R <: Ruling](ruling: R, delegate: DamageHealPromptControllerDelegate[R]) extends RulingPromptController[R] {

  private var result: Option[Int] = None

  def extractDecision(): Decision[R] = if (result.isDefined) delegate.buildDecision(ruling, result.get) else null

  def hasAnswer: Boolean = result.isDefined

  def handleAccept(value: Return): Boolean = {
    value match {
      case TextFieldValuePanel.Return(v) =>
        result = v.map(x => x.toInt)
        result.isDefined
      case _ =>
        false
    }
  }

  def decoratePanel(panel: ValuePanel[_]) = {
    val dhPanel = panel.asInstanceOf[DamageHealValuePanel]
    val default = delegate.startValue(ruling)
    if (default.isDefined) dhPanel.setInputValue(default.get.toString)
    if (result.isDefined) dhPanel.setValue(result.map(x => x.toString))
  }

  def panelIdentity(): String = delegate.panelId

  def prompt(): String = delegate.prompt(ruling)
}