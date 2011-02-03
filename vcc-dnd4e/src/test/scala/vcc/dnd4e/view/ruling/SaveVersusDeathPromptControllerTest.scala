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
import vcc.dnd4e.domain.tracker.common.{CombatantID, SaveVersusDeathRuling, SaveVersusDeathDecision}
import vcc.infra.prompter.EnumerationValuePanel

class SaveVersusDeathPromptControllerTest extends SpecificationWithJUnit with Mockito {

  import SaveVersusDeathDecision._

  val mPanel = mock[EnumerationValuePanel[SaveVersusDeathDecision.type]]
  val comb = CombatantID("Z")

  val ruling = SaveVersusDeathRuling(comb)

  "SaveVersusDeathPromptController" should {
    val controller = new SaveVersusDeathPromptController(ruling)
    "provide the correct prompt" in {
      controller.prompt must_== "Saving throw versus death"
    }

    "provide the correct prompt" in {
      controller.panelIdentity must_== RulingDialog.SaveVersusDeathPanelIdentity
    }

    "decorate to none on first" in {
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(None)
    }

    "sending None as Return must not be accepted" in {
      controller.decoratePanel(mPanel)
      controller.handleAccept(EnumerationValuePanel.Value(None)) must beFalse
    }

    "decorate to Saved on second time" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveVersusDeathDecision.Saved))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(SaveVersusDeathDecision.Saved))
    }

    "decorate to Failed on second time" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveVersusDeathDecision.Failed))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(SaveVersusDeathDecision.Failed))
    }

    "decorate to Save And Heal on second time" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveVersusDeathDecision.SaveAndHeal))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(SaveVersusDeathDecision.SaveAndHeal))
    }

    "once value Yes return correct decision" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveVersusDeathDecision.Saved)))
      controller.hasAnswer must beTrue
      controller.extractDecision() must_== SaveVersusDeathDecision(ruling, SaveVersusDeathDecision.Saved)
    }

    "once value No return correct decision" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveVersusDeathDecision.Failed)))
      controller.hasAnswer must beTrue
      controller.extractDecision() must_== SaveVersusDeathDecision(ruling, SaveVersusDeathDecision.Failed)
    }
    "once value No return correct decision" in {
      controller.handleAccept(EnumerationValuePanel.Value(Some(SaveVersusDeathDecision.SaveAndHeal)))
      controller.hasAnswer must beTrue
      controller.extractDecision() must_== SaveVersusDeathDecision(ruling, SaveVersusDeathDecision.SaveAndHeal)
    }
  }
}