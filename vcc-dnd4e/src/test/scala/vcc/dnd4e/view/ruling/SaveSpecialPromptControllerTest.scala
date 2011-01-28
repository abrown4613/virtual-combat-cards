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
import vcc.dnd4e.domain.tracker.common.{SaveEffectSpecialDecision, SaveEffectSpecialRuling}

class SaveSpecialPromptControllerTest extends SpecificationWithJUnit with Mockito {

  import vcc.dnd4e.view.ruling.SaveOrChangeValuePanel.Value
  import SaveEffectSpecialDecision._

  val mPanel = mock[SaveOrChangeValuePanel]

  val rulingProgress1 = SaveEffectSpecialRuling(null, "bad->worst ->even worst")

  "SaveSpecialPromptController" should {
    val controller = new SaveSpecialPromptController(rulingProgress1)
    "provide the correct prompt" in {
      controller.prompt must_== "Save: bad->worst ->even worst"
    }

    "ask for appropriate panel" in {
      controller.panelIdentity must_== SaveOrChangeValuePanel.Identity
    }

    "decorate to none on first" in {
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(None)
      there was one(mPanel).setNewCondition("worst -> even worst")
    }

    "sending None as Return must not be accepted" in {
      controller.decoratePanel(mPanel)
      controller.handleAccept(Value(None)) must beFalse
    }

    "decorate to Saved on second time" in {
      controller.handleAccept(Value(Some(Saved))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(Saved))
    }

    "decorate to Changed on second time" in {
      controller.handleAccept(Value(Some(Changed("stoned")))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(Changed("stoned")))
      there was one(mPanel).setNewCondition("stoned")
    }
    "decorate to original new condition when moving from changed to saved " in {
      controller.handleAccept(Value(Some(Changed("stoned")))) must beTrue
      controller.handleAccept(Value(Some(Saved))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setNewCondition("worst -> even worst")
    }

    "once value Saved return correct decision" in {
      controller.handleAccept(Value(Some(Saved)))
      controller.extractDecision() must_== SaveEffectSpecialDecision(rulingProgress1, SaveEffectSpecialDecision.Saved)
    }

    "once value Changed return correct decision" in {
      controller.handleAccept(Value(Some(Changed("stoned"))))
      controller.extractDecision() must_== SaveEffectSpecialDecision(rulingProgress1, SaveEffectSpecialDecision.Changed("stoned"))
    }

    "no value set should return null" in {
      controller.extractDecision() must beNull
    }

  }

  "SaveSpecialPromptController reformat" should {
    val controller = new SaveSpecialPromptController(rulingProgress1)

    "do nothing if has no arrow or slash" in {
      controller.progressCondition("same") must_== "same"
    }

    "skip first arrow and trim, join with arrow" in {
      controller.progressCondition(" one -> two /three") must_== "two -> three"
    }
    "skip first slash and trim joing with arrow" in {
      controller.progressCondition(" one / two     /  three") must_== "two -> three"
    }
  }

}
