/*
 * Copyright (C) 2008-2010 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.domain.tracker.common

import org.specs.SpecificationWithJUnit
import vcc.controller.{PendingRuling, Ruling, Decision}
import vcc.controller.message.TransactionalAction
import vcc.dnd4e.domain.tracker.common.Command.{UpdateEffectCondition, CancelEffect}

class DomainRulingTest extends SpecificationWithJUnit {
  private val eid = EffectID(CombatantID("A"), 1)

  "SaveEffectQuestion" should {
    val se = SaveEffectRuling(eid, "slowed")
    val se2 = SaveEffectRuling(eid, "slowed -> new effect")
    val ses = SaveEffectSpecialRuling(eid, "bad -> worst")
    val ses2 = SaveEffectSpecialRuling(eid, "bad -> even worst")
    val pending: PendingRuling[List[TransactionalAction]] = new PendingRuling(se)

    "Saved is a valid answer" in {
      val saved = SaveEffectDecision(se, true)
      val saved2 = SaveEffectDecision(se2, true)
      se.isValidDecision(saved) must beTrue
      se2.isValidDecision(saved2) must beTrue
    }

    "Failed save is only valid on normal save" in {
      val saved = SaveEffectDecision(se, false)
      val saved2 = SaveEffectDecision(se, false)
      se.isValidDecision(saved) must beTrue
      se2.isValidDecision(saved2) must beFalse
    }

    "Change is only valid on special save" in {
      val saved = SaveEffectSpecialDecision(ses, Option("new effect"))
      val saved2 = SaveEffectSpecialDecision(ses2, None)
      ses.isValidDecision(saved) must beTrue
      ses.isValidDecision(saved2) must beFalse
    }

    "PendingRuling should provide valid None on wrong operation" in {
      val saved = SaveEffectDecision(se2, true)
      pending.processDecision(saved) must_== None
    }
    "PendingRuling should provide valid save on True" in {
      val saved = SaveEffectDecision(se, true)
      pending.processDecision(saved) must_== Some(List(CancelEffect(eid)))
    }

    "PendingRuling should provide no action on failed save" in {
      val saved = SaveEffectDecision(se, false)
      pending.processDecision(saved) must_== Some(Nil)
    }

    "PendingRuling should provide update on failed and changed condition" in {
      val pending2: PendingRuling[List[TransactionalAction]] = new PendingRuling(ses2)
      val saved = SaveEffectSpecialDecision(ses2, Some("new effect"))
      pending2.processDecision(saved) must_== Some(List(UpdateEffectCondition(eid, Effect.Condition.Generic("new effect", false))))
    }
  }

  private def testSet() = {
    val qna: List[Decision[_ <: Ruling]] = List(
      RegenerateByDecision(RegenerateByRuling(eid, "regenerate 5"), 5),
      OngoingDamageDecision(OngoingDamageRuling(eid, "ongoing 5"), 5),
      SustainEffectDecision(SustainEffectRuling(eid, "what to sustain"), true)
    )
    qna
  }

  testSet.foreach{
    ans =>
      ("match " + (ans.ruling.getClass.getSimpleName) + " to " + (ans.getClass.getSimpleName)) in {
        ans.ruling.isValidDecision(ans) must beTrue
      }
  }


}

