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

import org.specs.mock.Mockito
import vcc.controller.{Ruling, Decision}
import org.specs.{SpecificationWithJUnit}
import vcc.infra.prompter.{TextFieldValuePanel}
import vcc.dnd4e.domain.tracker.common.{RegenerateByDecision, RegenerateByRuling, OngoingDamageDecision, OngoingDamageRuling}

class DamageHealPromptControllerTest extends SpecificationWithJUnit with Mockito {

  private val mDelegate = mock[DamageHealPromptControllerDelegate[Ruling]]

  private val mDecision = mock[Decision[Ruling]]
  private val mRuling = mock[Ruling]
  private val mPanel = mock[DamageHealValuePanel]
  private val mDefaults = mock[Ruling => (String, Int)]

  mDelegate.panelId returns "someId"
  mDelegate.prompt(mRuling) returns "prefix: bad things"
  mDelegate.startValue(mRuling) returns Some(7)
  mDelegate.buildDecision(mRuling, 10) returns mDecision
  mDelegate.buildDecision(mRuling, 7) returns mDecision

  private val controller = new DamageHealPromptController(mRuling, mDelegate)

  "a DamageHealPromptController" should {

    "call defaultGenerator" in {
      controller.decoratePanel(mPanel)
      there was one(mDelegate).startValue(mRuling)
      controller.hasAnswer must beFalse
    }

    "provide correct panelId" in {
      controller.panelIdentity must_== "someId"
    }

    "provide the correct prompt" in {
      controller.prompt must_== "prefix: bad things"
    }

    "decorate to default on first" in {
      controller.decoratePanel(mPanel)
      there was one(mPanel).setInputValue("7")
      there was no(mPanel).setValue(any)
    }

    "sending None as Return must not be accepted" in {
      controller.handleAccept(TextFieldValuePanel.Return(None)) must beFalse
      controller.hasAnswer must beFalse
    }

    "set value to 10 must be accepted" in {
      controller.handleAccept(TextFieldValuePanel.Return(Some("10")))
      controller.hasAnswer must beTrue
    }

    "decorate to 0 on second time" in {
      controller.handleAccept(TextFieldValuePanel.Return(Some("10")))
      controller.handleAccept(TextFieldValuePanel.Return(Some("0")))
      controller.decoratePanel(mPanel)
      there was one(mPanel).setValue(Some("0"))
    }

    "extract default value will generate null" in {
      controller.extractDecision() must beNull
      there was no(mDelegate).buildDecision(mRuling, 7)
    }

    "no decision if internal value is None" in {
      controller.handleAccept(TextFieldValuePanel.Return(None)) must beFalse
      controller.extractDecision() must beNull
      there was no(mDelegate).buildDecision(any, any)
    }

    "extract defined value" in {
      controller.handleAccept(TextFieldValuePanel.Return(Some("10")))
      controller.extractDecision() must_== mDecision
      there was one(mDelegate).buildDecision(mRuling, 10)
    }
  }

  "OngoingPromptControllerDelegate" should {
    val delegate = OngoingPromptControllerDelegate
    val ruling = OngoingDamageRuling(null, "ongoing 8 fire", 8)
    delegateSpecification(OngoingPromptControllerDelegate, ruling, "ongoing 8 fire", Some(8), OngoingDamageDecision(ruling, 10))
  }

  "RegeneratePromptControllerDelegate" should {
    val ruling = RegenerateByRuling(null, "regenerate 6 while bloodied", 6)
    delegateSpecification(RegeneratePromptControllerDelegate, ruling, "regenerate 6 while bloodied", Some(6), RegenerateByDecision(ruling, 10))
  }

  def delegateSpecification[R <: Ruling](delegate: DamageHealPromptControllerDelegate[R], ruling: R, prompt: String, init: Option[Int], decision: Decision[R]) {
    "build correct answer" in {
      delegate.buildDecision(ruling, 10) must_== decision
    }

    "provide correct prompt" in {
      delegate.prompt(ruling) must_== prompt
    }

    "provide correct initial value" in {
      delegate.startValue(ruling) must_== init
    }
  }
}


