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

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import vcc.dnd4e.domain.tracker.common.{SaveEffectDecision, SaveEffectRuling}
import vcc.infra.prompter.{EnumerationValuePanel}
import vcc.dnd4e.tracker.common.{CombatantID, EffectID}
import org.specs2.specification.Scope

class SaveEffectRulingPromptControllerTest extends SpecificationWithJUnit with Mockito {

  trait context extends Scope {
    val ruling = SaveEffectRuling(EffectID(CombatantID("A"), 10), "bad things")
    val mPanel = mock[EnumerationValuePanel[SaveEffectDecision.type]]
    val controller = new SimpleSavePromptController(ruling)
  }

  //FIXME: Mock fail on 1.6-SNAPSHOT and scala 2.9.0 need to fix
  "SimpleSavePromptController" should {
    "provide the correct prompt" in new context {
      controller.prompt must_== "Save: bad things"
    }

    "ask for appropriate panel" in new context {
      controller.panelIdentity must_== RulingDialog.SimpleSavePanelIdentity
    }

    "decorate to none on first" in new context {
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(None)

    }.pendingUntilFixed("Mock fix")

    "sending None as Return must not be accepted" in new context {
      controller.decoratePanel(mPanel)
      controller.handleAccept(EnumerationValuePanel.Value(None)) must beFalse
    }

    "decorate to yes on second time" in new context {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveEffectDecision.Saved))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(SaveEffectDecision.Saved))
    }.pendingUntilFixed("Mock fix")

    "decorate to No on second time" in new context {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveEffectDecision.Failed))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(SaveEffectDecision.Failed))
    }.pendingUntilFixed("Mock fix")

    "once value Yes return correct decision" in new context {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveEffectDecision.Saved)))
      controller.hasAnswer must beTrue
      controller.extractDecision() must_== SaveEffectDecision(ruling, SaveEffectDecision.Saved)
    }

    "once value No return correct decision" in new context {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveEffectDecision.Failed)))
      controller.hasAnswer must beTrue
      controller.extractDecision() must_== SaveEffectDecision(ruling, SaveEffectDecision.Failed)
    }
  }
}
