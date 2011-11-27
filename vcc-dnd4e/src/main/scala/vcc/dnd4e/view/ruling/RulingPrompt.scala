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
    promptUserAndCollectResult(new RulingPrompt(context))
  }

  private def promptUserAndCollectResult(prompt: RulingPrompt): List[Ruling[CombatState, _, _]] = {
    if (PromptDialog.promptUserAndDismiss(prompt.createModel()))
      prompt.generateResult()
    else
      Nil
  }

  private[ruling] def buildEffectProgression(effectDescription: String): String = {
    val effects = ConditionMatcher.splitProgression(effectDescription)
    if (effects.length > 1) effects.tail.mkString(" -> ") else effects(0)
  }
}

class RulingPrompt private(context: RulingContext[CombatState]) {

  private val panels = createPanels()

  def getPanels = panels.map(_.panel)

  def createModel(): PromptDialog.Model = {
    StaticModel(
      translateCommandToTitle(),
      getPanels)
  }

  def generateResult(): List[Ruling[CombatState, _, _]] = {
    panels.map(_.bindAnswer())
  }

  private class RulingPanelWrapper[R <: Ruling[CombatState, D, R], D](ruling: Ruling[CombatState, D, R], val panel: RadioPromptPanel[D]) {
    def bindAnswer(): Ruling[CombatState, D, R] = {
      ruling.withDecision(panel.response.get)
    }
  }

  private def createPanels(): List[RulingPanelWrapper[_, _]] = {
    context.rulingNeedingDecision.map(generateRulingPanel)
  }

  private def generateRulingPanel(ruling: Ruling[CombatState, _, _]): RulingPanelWrapper[_, _] = {
    ruling match {
      case sustain@SustainEffectRuling(eid, _) =>
        makeRulingPromptPanel(sustain, makeTitleFromEffect(" - Sustain effect: ", eid),
          ("Sustain", SustainEffectRulingResult.Sustain),
          ("Cancel", SustainEffectRulingResult.Cancel))
      case save@SaveRuling(eid, _) =>
        makeRulingPromptPanel(save, makeTitleFromEffect(" - Save against: ", eid),
          ("Saved", SaveRulingResult.Saved),
          ("Failed save", SaveRulingResult.Failed))
      case save@SaveVersusDeathRuling(combId, _) =>
        makeRulingPromptPanel(save, actingCombatantName() + " - Save versus Death",
          ("Saved", SaveVersusDeathResult.Saved),
          ("Saved and Heal (1 HP)", SaveVersusDeathResult.SaveAndHeal),
          ("Failed save", SaveVersusDeathResult.Failed))
      case save@SaveSpecialRuling(eid, _) =>
        val progression = RulingPrompt.buildEffectProgression(getEffectDescription(eid))
        makeRulingPromptPanel[SaveSpecialRuling, SaveSpecialRulingResult](save,
          makeTitleFromEffect(" - Save against: ", eid),
          ("Saved", SaveSpecialRulingResult.Saved),
          ("Failed and change to: " + progression, SaveSpecialRulingResult.Changed(progression)))

      case regen@RegenerationRuling(eid, _) =>
        val ConditionMatcher.FirstRegenerate(effectName, hint) = getEffectDescription(eid)
        makeRulingPromptPanel(regen,
          actingCombatantName() + " - Regeneration: " + effectName,
          ("Regenerate " + hint + " HP", hint),
          ("Skip", 0))
      case ongoing@OngoingDamageRuling(eid, _) =>
        val ConditionMatcher.FirstOngoing(effectName, hint) = getEffectDescription(eid)
        makeRulingPromptPanel(ongoing,
          actingCombatantName() + " - Ongoing Damage: " + effectName,
          ("Take " + hint + " damage", hint),
          ("Skip", 0))
    }
  }

  private def getEffectDescription(eid: EffectID): String = {
    val effect = context.state.roster.combatant(eid.combId).effects.find(eid)
    effect.map(e => e.condition.description).getOrElse("Missing effect?")
  }

  private def makeRulingPromptPanel[R <: Ruling[CombatState, D, R], D](ruling: R, title: String, choices: (String, D)*) = {
    new RulingPanelWrapper[R, D](ruling,
      new RadioPromptPanel[D](title, choices.map(p => RadioPromptPanel.Choice(p._1, p._2)): _*))
  }

  private def makeTitleFromEffect(infix: String, eid: EffectID): String = {
    actingCombatantName() + infix + getEffectDescription(eid)
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