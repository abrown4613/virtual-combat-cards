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

import vcc.dnd4e.tracker.common._
import vcc.dnd4e.view.dialog.PromptDialog.StaticModel
import vcc.dnd4e.tracker.command.{EndRoundCommand, StartRoundCommand}
import vcc.dnd4e.view.dialog.{RadioPromptPanel, PromptDialog}
import vcc.tracker.{Ruling, RulingContext}
import vcc.dnd4e.tracker.ruling._

object RulingPrompt {
  def promptUser(context: RulingContext[CombatState]): List[Ruling[CombatState, _, _]] = {
    val prompt = new RulingPrompt(context)
    val panels = prompt.getPanels
    PromptDialog.promptUserAndDismiss(StaticModel(prompt.translateCommandToTitle(), panels), null)
    prompt.generateResult()
  }

  private[ruling] def buildEffectProgression(effectDescription: String):String= {
    val effects = ConditionMatcher.splitProgression(effectDescription)
    if (effects.length > 1) effects.tail.mkString(" -> ") else effects(0)
  }
}

class RulingPrompt private(context: RulingContext[CombatState]) {

  private val panels = createPanels()

  def getPanels = panels.map(_.panel)

  def generateResult(): List[Ruling[CombatState, _, _]] = {
    panels.map(_.bindAnswer())
  }

  private class RulingPanelWrapper[D, R <: Ruling[CombatState, D, R]](ruling: Ruling[CombatState, D, R], val panel: RadioPromptPanel[D]) {
    def bindAnswer(): Ruling[CombatState, D, R] = {
      ruling.withDecision(panel.response.get)
    }
  }

  private def createPanels(): List[RulingPanelWrapper[_, _]] = {
    context.rulingNeedingDecision.map(generateRulingPanel)
  }

  private def getEffectDescription(eid: EffectID): String = {
    val effect = context.state.roster.combatant(eid.combId).effects.find(eid)
    effect.map(e => e.condition.description).getOrElse("Missing effect?")
  }

  private def generateRulingPanel(ruling: Ruling[CombatState, _, _]): RulingPanelWrapper[_, _] = {
    ruling match {
      case sustain@SustainEffectRuling(eid, _) =>
        new RulingPanelWrapper(sustain,
          new RadioPromptPanel[SustainEffectRulingResult.Value](
            actingCombatantName() + " - Sustain effect: " + getEffectDescription(eid),
            RadioPromptPanel.Choice("Sustain", SustainEffectRulingResult.Sustain),
            RadioPromptPanel.Choice("Cancel", SustainEffectRulingResult.Cancel)))
      case save@SaveRuling(eid, _) =>
        new RulingPanelWrapper(save,
          new RadioPromptPanel[SaveRulingResult.Value](
            actingCombatantName() + " - Save against: " + getEffectDescription(eid),
            RadioPromptPanel.Choice("Saved", SaveRulingResult.Saved),
            RadioPromptPanel.Choice("Failed save", SaveRulingResult.Failed)))
      case save@SaveVersusDeathRuling(combId, _) =>
        new RulingPanelWrapper(save,
          new RadioPromptPanel[SaveVersusDeathResult.Value](
            actingCombatantName() + " - Save versus Death",
            RadioPromptPanel.Choice("Saved", SaveVersusDeathResult.Saved),
            RadioPromptPanel.Choice("Saved and Heal (1 HP)", SaveVersusDeathResult.SaveAndHeal),
            RadioPromptPanel.Choice("Failed save", SaveVersusDeathResult.Failed)))
      case save@SaveSpecialRuling(eid, _) =>
        val progression = RulingPrompt.buildEffectProgression(getEffectDescription(eid))
        new RulingPanelWrapper(save,
          new RadioPromptPanel[SaveSpecialRulingResult](
            actingCombatantName() + " - Save against: " + getEffectDescription(eid),
            RadioPromptPanel.Choice("Saved", SaveSpecialRulingResult.Saved),
            RadioPromptPanel.Choice("Failed and change to: " + progression, SaveSpecialRulingResult.Changed(progression))))
    }
  }

  private def translateCommandToTitle(): String = {
    context.triggeringCommand match {
      case StartRoundCommand(ioi) => makeFormattedCombatantName(ioi) + " - Start Round"
      case EndRoundCommand(ioi) => makeFormattedCombatantName(ioi) + " - End Round"
      case _ => "Unknown event"
    }
  }

  private def actingCombatantName(): String = {
    context.triggeringCommand match {
      case StartRoundCommand(ioi) => makeFormattedCombatantName(ioi)
      case EndRoundCommand(ioi) => makeFormattedCombatantName(ioi)
      case _ => "No one is acting"
    }
  }

  private def makeFormattedCombatantName(ioi: InitiativeOrderID): String = {
    "[%s] %s".format(ioi.toLabelString, context.state.combatant(ioi.combId).name)
  }
}