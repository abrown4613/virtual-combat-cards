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
package vcc.dnd4e.domain.tracker.snapshot

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import org.specs2.specification.Scope
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.domain.tracker.common._
import vcc.controller.transaction.ChangeNotification

class CombatChangeAndStateSnapshotBuilderTest extends SpecificationWithJUnit
with Mockito with CombatStateSnapshotHelper[CombatStateWithChanges] {

  trait contextWithMock extends Scope {
    val mCSBuilder = mock[CombatStateSnapshotBuilder]
    val aBuilder = new CombatChangeAndStateSnapshotBuilder(mCSBuilder)
  }

  "aBuilder relating to the sub builder " should {
    "forward changes cycle" in new contextWithMock {
      processChanges(aBuilder, InitiativeOrderChange(List(ita0)))
      there was one(mCSBuilder).beginChanges() then
        one(mCSBuilder).processChange(InitiativeOrderChange(List(ita0))) then
        one(mCSBuilder).endChanges()
    }

    "request a valid snapshot" in new contextWithMock {
      val snap = aBuilder.getSnapshot()

      snap must not beNull;
      there was one(mCSBuilder).getSnapshot()
    }
  }

  "aBuilder capturing changes" should {

    "return same change for between begin" in new contextWithMock {
      aBuilder.beginChanges()
      val s1 = aBuilder.getSnapshot.changes
      aBuilder.getSnapshot.changes.eq(s1) must beTrue // Same between begins
    }

    "return different change for each begin" in new contextWithMock {
      aBuilder.beginChanges()
      val s1 = aBuilder.getSnapshot.changes
      aBuilder.beginChanges()
      aBuilder.getSnapshot.changes.eq(s1) must beFalse
    }

    "throw exception on a ChangeNotification it does not handle" in new contextWithMock {
      processChanges(aBuilder, mock[ChangeNotification]) must throwA[IllegalArgumentException]
    }

    "return a valid StateChange" in new contextWithMock {
      val snap = processChanges(aBuilder,
        InitiativeOrderChange(List(ita0)))
      snap.changes must not beNull;
    }

    "track change in order" in new contextWithMock {
      val snap = processChanges(aBuilder,
        InitiativeOrderChange(List(ita0, itb)))
      snap.changes.combatantsThatChanged(StateChange.combatant.Initiative) must_== Set(combA, combB)
      snap.changes.changes must_== Set(StateChange.combat.Order)
    }

    "track change in InitiativeTracker" in new contextWithMock {
      val snap = processChanges(aBuilder, InitiativeTrackerChange(ita0m))
      snap.changes.changesTo(combA) must_== Set(StateChange.combatant.Initiative)
      snap.changes.changes must_== Set()
    }

    "track change in HealthTracker" in new contextWithMock {
      val snap = processChanges(aBuilder, CombatantChange(combA, modHealth))
      snap.changes.changesTo(combA) must_== Set(StateChange.combatant.Health)
      snap.changes.changes must_== Set()
    }

    "track change in Comment" in new contextWithMock {
      val snap = processChanges(aBuilder, CombatantChange(combA, CombatantComment("abc")))
      snap.changes.changesTo(combA) must_== Set(StateChange.combatant.Comment)
      snap.changes.changes must_== Set()
    }

    "track change in Effects" in new contextWithMock {
      val nel = EffectList(combA, Nil).addEffect(combA, null, null)
      val snap = processChanges(aBuilder, CombatantChange(combA, nel))
      snap.changes.changesTo(combA) must_== Set(StateChange.combatant.Effects)
      snap.changes.changes must_== Set()
    }

    "track change in Definition" in new contextWithMock {
      val nDef = CombatantRosterDefinition(combA, "alias", null)
      val snap = processChanges(aBuilder, CombatantChange(combA, nDef))
      snap.changes.changesTo(combA) must_== Set(StateChange.combatant.Definition)
      snap.changes.changes must_== Set()
    }

    "track change in combat MetaData" in new contextWithMock {
      val snap = processChanges(aBuilder, CombatMetaDataChange(true, "fumes are active"))
      snap.changes.changes must_== Set(StateChange.combat.MetaData)
    }

    "track roster change" in new contextWithMock {
      val snap = processChanges(aBuilder,
        RosterChange(Map(
          combA -> generateCombatantAspectSet(combA),
          combB -> generateCombatantAspectSet(combB))))

      snap.changes.changes must_== Set(StateChange.combat.Roster)
      for (id <- List(combA, combB)) {
        snap.changes.changesTo(id) must_== Set(
          StateChange.combatant.Health,
          StateChange.combatant.Definition,
          StateChange.combatant.Effects,
          StateChange.combatant.Comment)
      }
    }

    "track change in combat change robin head" in new contextWithMock {
      val snap = processChanges(aBuilder, InitiativeOrderFirstChange(ioa0))
      snap.changes.changes must_== Set(StateChange.combat.OrderFirst)
    }
  }
}