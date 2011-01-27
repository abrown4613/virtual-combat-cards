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
import vcc.infra.prompter.RadioButtonValuePanel
import vcc.dnd4e.domain.tracker.common.{SaveEffectDecision, CombatantID, EffectID, SaveEffectRuling}

class SaveEffectRulingPromptControllerTest extends SpecificationWithJUnit with Mockito {

  val mPanel = mock[RadioButtonValuePanel]

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
      controller.handleAccept(RadioButtonValuePanel.Return(None)) must beFalse
    }

    "decorate to yes on second time" in {
      controller.handleAccept(RadioButtonValuePanel.Return(Some(0))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(0))
    }

    "decorate to No on second time" in {
      controller.handleAccept(RadioButtonValuePanel.Return(Some(1))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(1))
    }

    "once value Yes return correct decision" in {
      controller.handleAccept(RadioButtonValuePanel.Return(Some(0)))
      controller.extractDecision() must_== SaveEffectDecision(ruling, true)
    }

    "once value No return correct decision" in {
      controller.handleAccept(RadioButtonValuePanel.Return(Some(1)))
      controller.extractDecision() must_== SaveEffectDecision(ruling, false)
    }

  }
}
