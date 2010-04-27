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
import vcc.dnd4e.domain.tracker.common._

@RunWith(classOf[JUnitSuiteRunner])
class CombatStateSnapshotBuilderTest extends JUnit4(CombatStateSnapshotBuilderSpec)

object CombatStateSnapshotBuilderSpec extends Specification with CombatStateSnapshotHelper[CombatState] {
  var aBuilder: CombatStateSnapshotBuilder = null

  val emptyContext = beforeContext {
    aBuilder = new CombatStateSnapshotBuilder()
  }

  "aCombatStateSnapShotBuilder" ->- (emptyContext) should {

    "add an InitiativeTracker when followed by a InitiativeOrderChange" in {
      val snap = processChanges(aBuilder,
        InitiativeTrackerChange(ita0),
        InitiativeOrderChange(List(ita0)))

      snap.initiativeTrackerFromID(ioa0) must_== ita0
      snap.getInitiativeOrder() must_== List(ioa0)
    }

    "ignore an InitiativeTracker not in InitiativeOrderChange" in {
      processChanges(aBuilder, InitiativeOrderChange(List(itb)))

      val snap = processChanges(aBuilder, InitiativeTrackerChange(ita0))
      snap.initiativeTrackerFromID(ioa0) must throwAn[NoSuchElementException]
      snap.initiativeTrackerFromID(iob) must_== itb
      snap.getInitiativeOrder() must_== List(iob)
    }

    "update an InitiativeTracker in InitiativeOrderChange" in {
      processChanges(aBuilder, InitiativeOrderChange(List(ita0)))
      val snap = processChanges(aBuilder, InitiativeTrackerChange(ita0m))

      snap.initiativeTrackerFromID(ioa0) must_== ita0m
      snap.getInitiativeOrder() must_== List(ioa0)
    }

    "remove an InitiativeTracker on an InitiativeOrderChange" in {
      processChanges(aBuilder,
        InitiativeOrderChange(List(ita0, itb)))
      val snap = processChanges(aBuilder, InitiativeOrderChange(List(itb)))

      snap.initiativeTrackerFromID(ioa0) must throwA[NoSuchElementException]
      snap.getInitiativeOrder() must_== List(iob)
    }

    "store combatants on a RosterChange" in {
      val snap = processChanges(aBuilder,
        RosterChange(Map(
          combA -> generateCombatantAspectSet(combA),
          combB -> generateCombatantAspectSet(combB))))

      for (comb <- List(combA, combB)) {
        val cv = snap.combatantViewFromID(comb)
        cv must notBeNull
        cv.definition must_== generateCombatantRosterDefinition(comb)
        cv.definition.cid must_== comb
        cv.healthTracker must_== baseHealth
        cv.effects must_== generateEffects(comb)
        cv.comment must_== "dont"
      }
    }
  }

  "a builder with loaded context" ->- (beforeContext {
    aBuilder = new CombatStateSnapshotBuilder()
    processChanges(aBuilder,
      RosterChange(Map(
        combA -> generateCombatantAspectSet(combA),
        combB -> generateCombatantAspectSet(combB))),
      InitiativeOrderChange(List(ita0, itb)))
  }) should {
    "throw exception if not in roster" in {
      processChanges(aBuilder, CombatantChange(CombatantID("C"), CombatantComment("do it"))) must throwA[NoSuchElementException]
    }

    "update combatant comment" in {
      val snap = processChanges(aBuilder, CombatantChange(combA, CombatantComment("do it")))
      snap.combatantViewFromID(combA).comment must_== "do it"
    }

    "update health" in {
      val snap = processChanges(aBuilder, CombatantChange(combA, modHealth))
      snap.combatantViewFromID(combA).healthTracker must_== modHealth
    }

    "update effects" in {
      val nel = EffectList(combA, Nil).addEffect(combA, null, null)
      val snap = processChanges(aBuilder, CombatantChange(combA, nel))
      snap.combatantViewFromID(combA).effects must_== nel
    }

    "update definition" in {
      val nDef = CombatantRosterDefinition(combA, "alias", null)
      val snap = processChanges(aBuilder, CombatantChange(combA, nDef))
      snap.combatantViewFromID(combA).definition.alias must_== "alias"
    }

    "change roster head on InitiativeOrderFirstChange" in {
      val snap = processChanges(aBuilder, InitiativeOrderFirstChange(iob))
      snap.robinHead must_== Some(iob)
    }

    "change roster head to None on InitiativeOrderFirstChange(null)" in {
      val snap = processChanges(aBuilder, InitiativeOrderFirstChange(null))
      snap.robinHead must_== None
    }

    "throw exception if InitiativeOrderFirstChange points to something wrong" in {
      val ioc = InitiativeOrderID(CombatantID("C"), 0)
      processChanges(aBuilder, InitiativeOrderFirstChange(ioc)) must throwA[NoSuchElementException]
    }

    "update comment and combat state on CombatMetaDataChange" in {
      val snap = processChanges(aBuilder, CombatMetaDataChange(true, "fumes are active"))
      snap.combatComment must_== "fumes are active"
      snap.isCombatStarted must beTrue
    }

    "before a change to start combat, must not be in combat" in {
      aBuilder.getSnapshot().isCombatStarted must beFalse
    }
  }
}
