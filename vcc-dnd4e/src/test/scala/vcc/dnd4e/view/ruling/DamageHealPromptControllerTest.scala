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

import org.specs2.mock.Mockito
import org.specs2.specification.Scope
import vcc.controller.{Ruling, Decision}
import vcc.infra.prompter.{TextFieldValuePanel}
import vcc.dnd4e.domain.tracker.common.{RegenerateByDecision, RegenerateByRuling, OngoingDamageDecision, OngoingDamageRuling}
import org.specs2.mutable.{Specification, SpecificationWithJUnit}

class DamageHealPromptControllerTest extends SpecificationWithJUnit with Mockito {

  trait context extends Scope {
    val mDelegate = mock[DamageHealPromptControllerDelegate[Ruling]]
    val mDecision = mock[Decision[Ruling]]
    val mRuling = mock[Ruling]
    val mPanel = mock[DamageHealValuePanel]
    val mDefaults = mock[Ruling => (String, Int)]

    mDelegate.panelId returns "someId"
    mDelegate.prompt(mRuling) returns "prefix: bad things"
    mDelegate.startValue(mRuling) returns Some(7)
    mDelegate.buildDecision(mRuling, 10) returns mDecision
    mDelegate.buildDecision(mRuling, 7) returns mDecision

    val controller = new DamageHealPromptController(mRuling, mDelegate)
  }

  //FIXME: Mock fail on 1.6-SNAPSHOT and scala 2.9.0 need to fix
  "a DamageHealPromptController" should {

    "call defaultGenerator" in new context {
      controller.decoratePanel(mPanel)
      there was one(mDelegate).startValue(mRuling)
      controller.hasAnswer must beFalse
    }

    "provide correct panelId" in new context {
      controller.panelIdentity must_== "someId"
    }

    "provide the correct prompt" in new context {
      controller.prompt must_== "prefix: bad things"
    }

    "decorate to default on first" in new context {
      controller.decoratePanel(mPanel)
      there was one(mPanel).setInputValue("7")
      there was no(mPanel).setValue(any)
    }.pendingUntilFixed("Fix mock")

    "sending None as Return must not be accepted" in new context {
      controller.handleAccept(TextFieldValuePanel.Return(None)) must beFalse
      controller.hasAnswer must beFalse
    }

    "set value to 10 must be accepted" in new context {
      controller.handleAccept(TextFieldValuePanel.Return(Some("10")))
      controller.hasAnswer must beTrue
    }

    "decorate to 0 on second time" in new context {
      controller.handleAccept(TextFieldValuePanel.Return(Some("10")))
      controller.handleAccept(TextFieldValuePanel.Return(Some("0")))
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some("0"))
    }.pendingUntilFixed("Fix mock")

    "extract default value will generate null" in new context {
      controller.extractDecision() must beNull
      there was no(mDelegate).buildDecision(mRuling, 7)
    }

    "no decision if internal value is None" in new context {
      controller.handleAccept(TextFieldValuePanel.Return(None)) must beFalse
      controller.extractDecision() must beNull
      there was no(mDelegate).buildDecision(any, any)
    }

    "extract defined value" in new context {
      controller.handleAccept(TextFieldValuePanel.Return(Some("10")))
      controller.extractDecision() must_== mDecision
      there was one(mDelegate).buildDecision(mRuling, 10)
    }
  }

  {
    val ruling = OngoingDamageRuling(null, "ongoing 8 fire", 8)
    include(delegateSpecification("OngoingPromptControllerDelegate", OngoingPromptControllerDelegate, ruling, "ongoing 8 fire", Some(8), OngoingDamageDecision(ruling, 10)))
  }

  {
    val ruling = RegenerateByRuling(null, "regenerate 6 while bloodied", 6)
    include(delegateSpecification("RegeneratePromptControllerDelegate", RegeneratePromptControllerDelegate, ruling, "regenerate 6 while bloodied", Some(6), RegenerateByDecision(ruling, 10)))
  }

  def delegateSpecification[R <: Ruling](specTitle: String, delegate: DamageHealPromptControllerDelegate[R], ruling: R, prompt: String, init: Option[Int], decision: Decision[R]) = {
    new Specification {
      specTitle should {
        "build correct answer" in new context {
          delegate.buildDecision(ruling, 10) must_== decision
        }

        "provide correct prompt" in new context {
          delegate.prompt(ruling) must_== prompt
        }

        "provide correct initial value" in new context {
          delegate.startValue(ruling) must_== init
        }
      }
    }
  }
}


