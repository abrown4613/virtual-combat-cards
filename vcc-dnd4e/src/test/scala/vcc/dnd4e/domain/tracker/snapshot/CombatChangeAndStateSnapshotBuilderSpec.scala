/**
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
package vcc.dnd4e.domain.tracker.snapshot


import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import org.specs.mock.Mockito
import vcc.dnd4e.domain.tracker.common._
import vcc.controller.transaction.ChangeNotification

@RunWith(classOf[JUnitSuiteRunner])
class CombatChangeAndStateSnapshotBuilderTest extends JUnit4(CombatChangeAndStateSnapshotBuilderSpec)

object CombatChangeAndStateSnapshotBuilderSpec extends Specification
        with Mockito with CombatStateSnapshotHelper[CombatStateWithChanges] {
  var aBuilder: CombatChangeAndStateSnapshotBuilder = null
  var mCSBuilder: CombatStateSnapshotBuilder = null

  val contextWithMock = beforeContext {
    mCSBuilder = mock[CombatStateSnapshotBuilder]
    aBuilder = new CombatChangeAndStateSnapshotBuilder(mCSBuilder)
  }

  "aBuilder relating to the sub builder " ->- (contextWithMock) should {
    "forward changes cycle" in {
      processChanges(aBuilder, InitiativeOrderChange(List(ita0)))
      there was one(mCSBuilder).beginChanges() then
              one(mCSBuilder).processChange(InitiativeOrderChange(List(ita0))) then
              one(mCSBuilder).endChanges()
    }

    "request a valid snapshot" in {
      val snap = aBuilder.getSnapshot()

      snap must notBeNull
      there was one(mCSBuilder).getSnapshot()
    }
  }

  "aBuilder capturing changes" ->- (contextWithMock) should {

    "return same change for between begin" in {
      aBuilder.beginChanges()
      val s1 = aBuilder.getSnapshot.changes
      aBuilder.getSnapshot.changes.eq(s1) must beTrue // Same between begins
    }

    "return different change for each begin" in {
      aBuilder.beginChanges()
      val s1 = aBuilder.getSnapshot.changes
      aBuilder.beginChanges()
      aBuilder.getSnapshot.changes.eq(s1) must beFalse
    }

    "throw exception on a ChangeNotification it does not handle" in {
      processChanges(aBuilder, mock[ChangeNotification]) must throwA[IllegalArgumentException]
    }

    "return a valid StateChange" in {
      val snap = processChanges(aBuilder,
        InitiativeOrderChange(List(ita0)))
      snap.changes must notBeNull
    }

    "track change in order" in {
      val snap = processChanges(aBuilder,
        InitiativeOrderChange(List(ita0, itb)))
      snap.changes.combatantsThatChanged(StateChange.combatant.Initiative) must_== Set(combA, combB)
      snap.changes.changes must_== Set(StateChange.combat.Order)
    }

    "track change in InitiativeTracker" in {
      val snap = processChanges(aBuilder, InitiativeTrackerChange(ita0m))
      snap.changes.changesTo(combA) must_== Set(StateChange.combatant.Initiative)
      snap.changes.changes must_== Set()
    }

    "track change in HealthTracker" in {
      val snap = processChanges(aBuilder, CombatantChange(combA, modHealth))
      snap.changes.changesTo(combA) must_== Set(StateChange.combatant.Health)
      snap.changes.changes must_== Set()
    }

    "track change in Comment" in {
      val snap = processChanges(aBuilder, CombatantChange(combA, CombatantComment("abc")))
      snap.changes.changesTo(combA) must_== Set(StateChange.combatant.Comment)
      snap.changes.changes must_== Set()
    }

    "track change in Effects" in {
      val nel = EffectList(combA, Nil).addEffect(combA, null, null)
      val snap = processChanges(aBuilder, CombatantChange(combA, nel))
      snap.changes.changesTo(combA) must_== Set(StateChange.combatant.Effects)
      snap.changes.changes must_== Set()
    }

    "track change in Definition" in {
      val nDef = CombatantRosterDefinition(combA, "alias", null)
      val snap = processChanges(aBuilder, CombatantChange(combA, nDef))
      snap.changes.changesTo(combA) must_== Set(StateChange.combatant.Definition)
      snap.changes.changes must_== Set()
    }

    "track change in combat MetaData" in {
      val snap = processChanges(aBuilder, CombatMetaDataChange(true, "fumes are active"))
      snap.changes.changes must_== Set(StateChange.combat.MetaData)
    }

    "track roster change" in {
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

    "track change in combat change robin head" in {
      val snap = processChanges(aBuilder, InitiativeOrderFirstChange(ioa0))
      snap.changes.changes must_== Set(StateChange.combat.OrderFirst)
    }

  }
}