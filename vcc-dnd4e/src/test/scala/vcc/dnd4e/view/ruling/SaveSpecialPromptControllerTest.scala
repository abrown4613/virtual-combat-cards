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

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import org.specs2.specification.{Scope}
import vcc.dnd4e.domain.tracker.common.{SaveEffectSpecialDecision, SaveEffectSpecialRuling}
import vcc.infra.prompter.ValuePanel

class SaveSpecialPromptControllerTest extends SpecificationWithJUnit with Mockito {

  import vcc.dnd4e.view.ruling.SaveOrChangeValuePanel.Value
  import SaveEffectSpecialDecision._

  trait context extends Scope {
    val mPanel = mock[ValuePanel[SaveEffectSpecialDecision.Result]]
    val rulingProgress1 = SaveEffectSpecialRuling(null, "bad->worst ->even worst")
    val controller = new SaveSpecialPromptController(rulingProgress1)
  }

  "SaveSpecialPromptController" should {
    "provide the correct prompt" in new context {
      controller.prompt must_== "Save: bad->worst ->even worst"
    }

    "ask for appropriate panel" in new context {
      controller.panelIdentity must_== SaveOrChangeValuePanel.Identity
    }

    "decorate to none on first" in new context {
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(None)
      there was one(mPanel).setField("NewCondition", "worst -> even worst")
    }

    "sending None as Return must not be accepted" in new context {
      controller.decoratePanel(mPanel)
      controller.handleAccept(Value(None)) must beFalse
    }

    "decorate to Saved on second time" in new context {
      controller.handleAccept(Value(Some(Saved))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(Saved))
      there was one(mPanel).setField("NewCondition", "worst -> even worst")
      controller.hasAnswer must beTrue
    }

    "decorate to Changed on second time" in new context {
      controller.handleAccept(Value(Some(Changed("stoned")))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some(Changed("stoned")))
      there was one(mPanel).setField("NewCondition", "stoned")
      controller.hasAnswer must beTrue
    }

    "decorate to original new condition when moving from changed to saved " in new context {
      controller.handleAccept(Value(Some(Changed("stoned")))) must beTrue
      controller.handleAccept(Value(Some(Saved))) must beTrue
      controller.decoratePanel(mPanel)
      there was one(mPanel).setField("NewCondition", "worst -> even worst")
    }

    "once value Saved return correct decision" in new context {
      controller.handleAccept(Value(Some(Saved)))
      controller.extractDecision() must_== SaveEffectSpecialDecision(rulingProgress1, SaveEffectSpecialDecision.Saved)
    }

    "once value Changed return correct decision" in new context {
      controller.handleAccept(Value(Some(Changed("stoned"))))
      controller.extractDecision() must_== SaveEffectSpecialDecision(rulingProgress1, SaveEffectSpecialDecision.Changed("stoned"))
    }

    "no value set should return null" in new context {
      controller.extractDecision() must beNull
    }

  }

  "SaveSpecialPromptController reformat" should {

    "do nothing if has no arrow or slash" in new context {
      controller.progressCondition("same") must_== "same"
    }

    "skip first arrow and trim, join with arrow" in new context {
      controller.progressCondition(" one -> two /three") must_== "two -> three"
    }
    "skip first slash and trim joing with arrow" in new context {
      controller.progressCondition(" one / two     /  three") must_== "two -> three"
    }
  }
}
