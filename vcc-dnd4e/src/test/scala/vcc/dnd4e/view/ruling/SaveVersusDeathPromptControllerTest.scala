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
import org.specs2.specification.Scope
import vcc.dnd4e.domain.tracker.common.{SaveVersusDeathRuling, SaveVersusDeathDecision}
import vcc.infra.prompter.EnumerationValuePanel
import vcc.dnd4e.tracker.common.CombatantID

class SaveVersusDeathPromptControllerTest extends SpecificationWithJUnit with Mockito {

  import SaveVersusDeathDecision._

  trait context extends Scope {
    val mPanel = mock[EnumerationValuePanel[SaveVersusDeathDecision.type]]
    val comb = CombatantID("Z")
    val ruling = SaveVersusDeathRuling(comb)
    val controller = new SaveVersusDeathPromptController(ruling)
  }

  //FIXME Mocks dont work on scala 2.9.0
  "SaveVersusDeathPromptController" should {
    "provide the correct prompt" in new context {
      controller.prompt must_== "Saving throw versus death"
    }

    "provide the correct prompt" in new context {
      controller.panelIdentity must_== RulingDialog.SaveVersusDeathPanelIdentity
    }

    "decorate to none on first" in new context {
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(None)
    }.pendingUntilFixed("Mock fix")

    "sending None as Return must not be accepted" in new context {
      controller.decoratePanel(mPanel)
      controller.handleAccept(EnumerationValuePanel.Value(None)) must beFalse
    }

    "decorate to Saved on second time" in new context {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveVersusDeathDecision.Saved))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(SaveVersusDeathDecision.Saved))
    }.pendingUntilFixed("Mock fix")

    "decorate to Failed on second time" in new context {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveVersusDeathDecision.Failed))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(SaveVersusDeathDecision.Failed))
    }.pendingUntilFixed("Mock fix")

    "decorate to Save And Heal on second time" in new context {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveVersusDeathDecision.SaveAndHeal))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(SaveVersusDeathDecision.SaveAndHeal))
    }.pendingUntilFixed("Mock fix")

    "once value Yes return correct decision" in new context {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveVersusDeathDecision.Saved)))
      controller.hasAnswer must beTrue
      controller.extractDecision() must_== SaveVersusDeathDecision(ruling, SaveVersusDeathDecision.Saved)
    }

    "once value No return correct decision" in new context {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveVersusDeathDecision.Failed)))
      controller.hasAnswer must beTrue
      controller.extractDecision() must_== SaveVersusDeathDecision(ruling, SaveVersusDeathDecision.Failed)
    }
    "once value No return correct decision" in new context {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveVersusDeathDecision.SaveAndHeal)))
      controller.hasAnswer must beTrue
      controller.extractDecision() must_== SaveVersusDeathDecision(ruling, SaveVersusDeathDecision.SaveAndHeal)
    }
  }
}