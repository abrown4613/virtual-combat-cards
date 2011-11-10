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
package vcc.infra.prompter

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import org.specs2.specification.Scope
import vcc.controller.{Decision, Ruling}

class RulingBrokerTest extends SpecificationWithJUnit with Mockito {

  trait SomeRuling extends Ruling

  trait AnotherRuling extends Ruling

  trait context extends Scope {
    val ruling1 = mock[SomeRuling]
    val ruling2 = mock[AnotherRuling]
    val rulings = List(ruling1, ruling2)
    val mDialog = mock[MultiplePromptDialogController]
    val mTranslator = mock[RulingTranslatorService]
    val mPrompt1 = mock[RulingPromptController[SomeRuling]]
    val mPrompt2 = mock[RulingPromptController[AnotherRuling]]
    val mDecision1 = mock[Decision[SomeRuling]]
    val mDecision2 = mock[Decision[AnotherRuling]]
    mPrompt1.extractDecision returns (mDecision1)
    mPrompt2.extractDecision returns mDecision2
    mTranslator.promptForRuling(ruling1) returns mPrompt1
    mTranslator.promptForRuling(ruling2) returns mPrompt2
    val promptContext = "some action"

    val broker = new RulingBroker(mDialog, mTranslator)
  }

  "RulingBroker" should {
    "translate Ruling to Prompts using tranlation service" in new context {
      broker.promptRuling(promptContext, rulings)
      there was one(mTranslator).promptForRuling(ruling1) then
        one(mTranslator).promptForRuling(ruling2)
    }
    "sent promper prompts to dialog controller" in new context {
      broker.promptRuling(promptContext, rulings)
      there was one(mDialog).promptUser(promptContext, List(mPrompt1, mPrompt2))
    }

    "ignore decision if dialog cancelled" in new context {
      mDialog.promptUser(any, any) returns false
      broker.promptRuling(promptContext, rulings) must_== Nil
      there was no(mPrompt1).extractDecision()
      there was no(mPrompt2).extractDecision()
    }

    "extract decision from prompts if positive" in new context {
      mDialog.promptUser(any, any) returns true
      broker.promptRuling(promptContext, rulings)
      there was one(mPrompt1).extractDecision()
      there was one(mPrompt2).extractDecision()
    }
    "sent decision back" in new context {
      mDialog.promptUser(any, any) returns true
      broker.promptRuling(promptContext, rulings) must_== List(mDecision1, mDecision2)
    }
  }
}
