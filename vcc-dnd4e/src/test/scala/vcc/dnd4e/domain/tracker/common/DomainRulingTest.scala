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
import vcc.controller.{PendingRuling}
import vcc.controller.message.TransactionalAction
import vcc.dnd4e.domain.tracker.common.Command.{UpdateEffectCondition, CancelEffect}
import vcc.dnd4e.domain.tracker.common.Effect.Condition

class DomainRulingTest extends SpecificationWithJUnit {
  private val eid = EffectID(CombatantID("A"), 1)

  "SaveEffectQuestion" should {
    val se = SaveEffectRuling(eid, "slowed")
    val se2 = SaveEffectRuling(eid, "slowed -> new effect")
    val ses = SaveEffectSpecialRuling(eid, "bad -> worst")
    val ses2 = SaveEffectSpecialRuling(eid, "bad -> even worst")
    val pending: PendingRuling[List[TransactionalAction]] = new PendingRuling(se)

    "Saved is a valid answer" in {
      val saved = SaveEffectDecision(se, SaveEffectDecision.Failed)
      val saved2 = SaveEffectDecision(se2, SaveEffectDecision.Saved)
      se.isValidDecision(saved) must beTrue
      se2.isValidDecision(saved2) must beTrue
    }

    "Failed save is only valid on normal save" in {
      val saved = SaveEffectDecision(se, SaveEffectDecision.Failed)
      val saved2 = SaveEffectDecision(se, SaveEffectDecision.Failed)
      se.isValidDecision(saved) must beTrue
      se2.isValidDecision(saved2) must beFalse
    }

    "PendingRuling should provide valid None on wrong operation" in {
      val saved = SaveEffectDecision(se2, SaveEffectDecision.Saved)
      pending.processDecision(saved) must_== None
    }
    "PendingRuling should provide valid save on True" in {
      val saved = SaveEffectDecision(se, SaveEffectDecision.Saved)
      pending.processDecision(saved) must_== Some(List(CancelEffect(eid)))
    }

    "PendingRuling should provide no action on failed save" in {
      val saved = SaveEffectDecision(se, SaveEffectDecision.Failed)
      pending.processDecision(saved) must_== Some(Nil)
    }

    "fromEffect return null for bad incorrect type" in {
      SaveEffectRuling.fromEffect(Effect(eid, eid.combId, Condition.Generic("abc", false), Duration.Stance)) must beNull
      SaveEffectRuling.fromEffect(Effect(eid, eid.combId, Condition.Generic("abc", false), Duration.SaveEndSpecial)) must beNull
    }

    "fromEffect return valid Ruling for good effect" in {
      SaveEffectRuling.fromEffect(Effect(eid, eid.combId, Condition.Generic("abc", false), Duration.SaveEnd)) must_== SaveEffectRuling(eid, "abc")
    }
  }

  "SaveEffectSpecialRuling" should {

    val ses = SaveEffectSpecialRuling(eid, "bad -> worst")
    val ses2 = SaveEffectSpecialRuling(eid, "bad -> even worst")
    val pending: PendingRuling[List[TransactionalAction]] = new PendingRuling(ses)

    "Change is only valid on special save" in {
      val saved = SaveEffectSpecialDecision(ses, SaveEffectSpecialDecision.Changed("new effect"))
      val saved2 = SaveEffectSpecialDecision(ses2, SaveEffectSpecialDecision.Saved)
      ses.isValidDecision(saved) must beTrue
      ses.isValidDecision(saved2) must beFalse
    }

    "PendingRuling provides valid None on wrong operation" in {
      val saved = SaveEffectSpecialDecision(ses2, SaveEffectSpecialDecision.Saved)
      pending.processDecision(saved) must_== None
    }

    "PendingRuling provides valid None on wrong operation" in {
      val saved = SaveEffectSpecialDecision(ses, SaveEffectSpecialDecision.Saved)
      pending.processDecision(saved) must_== Some(List(CancelEffect(eid)))
    }

    "PendingRuling provides valid None on wrong operation" in {
      val saved = SaveEffectSpecialDecision(ses, SaveEffectSpecialDecision.Changed("new condition"))
      pending.processDecision(saved) must_== Some(List(UpdateEffectCondition(eid, Effect.Condition.Generic("new condition", false))))
    }

    "fromEffect return null for bad incorrect type" in {
      SaveEffectSpecialRuling.fromEffect(Effect(eid, eid.combId, Condition.Generic("abc", false), Duration.Stance)) must beNull
    }

    "fromEffect return valid Ruling for good effect" in {
      SaveEffectSpecialRuling.fromEffect(Effect(eid, eid.combId, Condition.Generic("abc", false), Duration.SaveEndSpecial)) must_== SaveEffectSpecialRuling(eid, "abc")
    }
  }

  "SaveVersusDeathRuling" should {
    val comb = CombatantID("G")
    val sRuling = SaveVersusDeathRuling(comb)
    val pending: PendingRuling[List[TransactionalAction]] = new PendingRuling(sRuling)

    "Accept all types of SaveRuling" in {
      sRuling.isValidDecision(SaveVersusDeathDecision(sRuling, SaveVersusDeathDecision.Failed)) must beTrue
      sRuling.isValidDecision(SaveVersusDeathDecision(sRuling, SaveVersusDeathDecision.Saved)) must beTrue
      sRuling.isValidDecision(SaveVersusDeathDecision(sRuling, SaveVersusDeathDecision.SaveAndHeal)) must beTrue
    }

    "PendingRuling should provide valid None on wrong operation" in {
      val saved = SaveEffectDecision(null, SaveEffectDecision.Saved)
      pending.processDecision(saved) must_== None
    }

    "PendingRuling should provide FailDeathSave action on failed save" in {
      val saved = SaveVersusDeathDecision(sRuling, SaveVersusDeathDecision.Failed)
      pending.processDecision(saved) must_== Some(List(Command.FailDeathSave(comb)))
    }

    "PendingRuling should provide FailDeathSave action on failed save" in {
      val saved = SaveVersusDeathDecision(sRuling, SaveVersusDeathDecision.SaveAndHeal)
      pending.processDecision(saved) must_== Some(List(Command.HealDamage(comb, 1)))
    }
  }

  "OngoingRuling" should {
    val sRuling = OngoingDamageRuling(eid, "ongoing 5 fire", 5)
    val pending: PendingRuling[List[TransactionalAction]] = new PendingRuling(sRuling)
    val decision = OngoingDamageDecision(sRuling, 10)

    "accept valid decision" in {
      sRuling.isValidDecision(decision) must beTrue
    }

    "reject other decision" in {
      sRuling.isValidDecision(OngoingDamageDecision(null, 5)) must beFalse
    }

    "skip zero result action" in {
      pending.processDecision(OngoingDamageDecision(sRuling, 0)) must_== Some(List())
    }

    "convert non zero result to damage action" in {
      pending.processDecision(OngoingDamageDecision(sRuling, 5)) must_== Some(List(Command.ApplyDamage(eid.combId, 5)))
    }
  }

  "RegenerateByRuling" should {
    val sRuling = RegenerateByRuling(eid, "regenarate 5 while bloodied", 5)
    val pending: PendingRuling[List[TransactionalAction]] = new PendingRuling(sRuling)

    "accept valid decision" in {
      sRuling.isValidDecision(RegenerateByDecision(sRuling, 0)) must beTrue
    }

    "reject other decision" in {
      sRuling.isValidDecision(RegenerateByDecision(null, 5)) must beFalse
    }

    "skip zero result action" in {
      pending.processDecision(RegenerateByDecision(sRuling, 0)) must_== Some(List())
    }

    "convert non zero result to damage action" in {
      pending.processDecision(RegenerateByDecision(sRuling, 15)) must_== Some(List(Command.HealDamage(eid.combId, 15)))
    }
  }


  "SustainEffectRuling" should {
    val comb = CombatantID("G")
    val sRuling = SustainEffectRuling(eid, "some nasty zone")
    val pending: PendingRuling[List[TransactionalAction]] = new PendingRuling(sRuling)

    "Accept all types of SaveRuling" in {
      sRuling.isValidDecision(SustainEffectDecision(sRuling, SustainEffectDecision.Sustain)) must beTrue
      sRuling.isValidDecision(SustainEffectDecision(sRuling, SustainEffectDecision.Cancel)) must beTrue
    }

    "PendingRuling should provide valid None on wrong operation" in {
      val saved = SaveEffectDecision(null, SaveEffectDecision.Saved)
      pending.processDecision(saved) must_== None
    }

    "PendingRuling should provide FailDeathSave action on failed save" in {
      val saved = SustainEffectDecision(sRuling, SustainEffectDecision.Cancel)
      pending.processDecision(saved) must_== Some(Nil)
    }

    "PendingRuling should provide FailDeathSave action on failed save" in {
      val saved = SustainEffectDecision(sRuling, SustainEffectDecision.Sustain)
      pending.processDecision(saved) must_== Some(List(Command.SustainEffect(eid)))
    }
  }
}

