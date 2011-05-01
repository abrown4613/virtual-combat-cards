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

import org.specs.SpecificationWithJUnit
import vcc.infra.prompter.EnumerationValuePanel
import org.specs.mock.Mockito
import vcc.dnd4e.domain.tracker.common.{SustainEffectRuling, SustainEffectDecision}
import vcc.dnd4e.tracker.common.{EffectID, CombatantID}

class SustainEffectPromptControllerTest extends SpecificationWithJUnit with Mockito {

  val mPanel = mock[EnumerationValuePanel[SustainEffectDecision.type]]
  val comb = CombatantID("Z")
  val eid = EffectID(comb, 10)

  val ruling = SustainEffectRuling(eid, "nasty zone")

  "SustainEffectPromptController" should {
    val controller = new SustainEffectPromptController(ruling)
    "provide the correct prompt" in {
      controller.prompt must_== "Sustain: nasty zone"
    }

    "provide the correct panelId" in {
      controller.panelIdentity must_== RulingDialog.SustainEffectPanelIdentity
    }

    "decorate to none on first" in {
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(None)
    }

    "sending None as Return must not be accepted" in {
      controller.decoratePanel(mPanel)
      controller.handleAccept(EnumerationValuePanel.Value(None)) must beFalse
    }

    "decorate to Sustain on second time" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SustainEffectDecision.Sustain))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(SustainEffectDecision.Sustain))
    }

    "decorate to Cancel on second time" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SustainEffectDecision.Cancel))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(SustainEffectDecision.Cancel))
    }

    "decorate to Sustain on second time" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SustainEffectDecision.Sustain))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(SustainEffectDecision.Sustain))
    }

    "once value Sustain return correct decision" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SustainEffectDecision.Sustain)))
      controller.hasAnswer must beTrue
      controller.extractDecision() must_== SustainEffectDecision(ruling, SustainEffectDecision.Sustain)
    }

    "once value Cancel return correct decision" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SustainEffectDecision.Cancel)))
      controller.hasAnswer must beTrue
      controller.extractDecision() must_== SustainEffectDecision(ruling, SustainEffectDecision.Cancel)
    }
  }
}