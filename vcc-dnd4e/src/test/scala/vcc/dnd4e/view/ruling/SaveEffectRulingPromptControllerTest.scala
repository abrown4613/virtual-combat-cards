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
import org.specs.mock.Mockito
import vcc.dnd4e.domain.tracker.common.{SaveEffectDecision, CombatantID, EffectID, SaveEffectRuling}
import vcc.infra.prompter.{EnumerationValuePanel}

class SaveEffectRulingPromptControllerTest extends SpecificationWithJUnit with Mockito {

  val mPanel = mock[EnumerationValuePanel[SaveEffectDecision.type]]

  val ruling = SaveEffectRuling(EffectID(CombatantID("A"), 10), "bad things")
  "SimpleSavePromptController" should {
    val controller = new SimpleSavePromptController(ruling)
    "provide the correct prompt" in {
      controller.prompt must_== "Save: bad things"
    }

    "ask for appropriate panel" in {
      controller.panelIdentity must_== RulingDialog.SimpleSavePanelIdentity
    }

    "decorate to none on first" in {
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(None)
    }

    "sending None as Return must not be accepted" in {
      controller.decoratePanel(mPanel)
      controller.handleAccept(EnumerationValuePanel.Value(None)) must beFalse
    }

    "decorate to yes on second time" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveEffectDecision.Saved))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(SaveEffectDecision.Saved))
    }

    "decorate to No on second time" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveEffectDecision.Failed))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(SaveEffectDecision.Failed))
    }

    "once value Yes return correct decision" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveEffectDecision.Saved)))
      controller.hasAnswer must beTrue
      controller.extractDecision() must_== SaveEffectDecision(ruling, SaveEffectDecision.Saved)
    }

    "once value No return correct decision" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveEffectDecision.Failed)))
      controller.hasAnswer must beTrue
      controller.extractDecision() must_== SaveEffectDecision(ruling, SaveEffectDecision.Failed)
    }
  }
}
