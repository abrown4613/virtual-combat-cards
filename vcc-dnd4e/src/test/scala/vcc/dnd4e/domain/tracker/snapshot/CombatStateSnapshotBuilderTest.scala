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
import org.specs2.specification.Scope
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.domain.tracker.common._

class CombatStateSnapshotBuilderTest extends SpecificationWithJUnit with CombatStateSnapshotHelper[SnapshotCombatState] {

  trait emptyContext extends Scope {
    val aBuilder = new CombatStateSnapshotBuilder()
  }

  "aCombatStateSnapShotBuilder" should {

    "add an InitiativeTracker when followed by a InitiativeOrderChange" in new emptyContext {
      val snap = processChanges(aBuilder,
        InitiativeTrackerChange(ita0),
        InitiativeOrderChange(List(ita0)))

      snap.initiativeTrackerFromID(ioa0) must_== ita0
      snap.getInitiativeOrder must_== List(ioa0)
    }

    "ignore an InitiativeTracker not in InitiativeOrderChange" in new emptyContext {
      processChanges(aBuilder, InitiativeOrderChange(List(itb)))

      val snap = processChanges(aBuilder, InitiativeTrackerChange(ita0))
      snap.initiativeTrackerFromID(ioa0) must throwAn[NoSuchElementException]
      snap.initiativeTrackerFromID(iob) must_== itb
      snap.getInitiativeOrder must_== List(iob)
    }

    "handle InitiativeOrderChangeFirst then InitiativeOrderChange" in new emptyContext {
      //This happens on an undo of a End Combat
      val snap = processChanges(aBuilder, InitiativeOrderFirstChange(ioa0), InitiativeOrderChange(List(itb, ita0)))
      snap.initiativeTrackerFromID(ioa0) must_== ita0
      snap.initiativeTrackerFromID(iob) must_== itb
      snap.getInitiativeOrder must_== List(iob, ioa0)
      snap.nextUp must_== Some(ioa0)
    }

    "update an InitiativeTracker in InitiativeOrderChange" in new emptyContext {
      processChanges(aBuilder, InitiativeOrderChange(List(ita0)))
      val snap = processChanges(aBuilder, InitiativeTrackerChange(ita0m))

      snap.initiativeTrackerFromID(ioa0) must_== ita0m
      snap.getInitiativeOrder must_== List(ioa0)
    }

    "remove an InitiativeTracker on an InitiativeOrderChange" in new emptyContext {
      processChanges(aBuilder,
        InitiativeOrderChange(List(ita0, itb)))
      val snap = processChanges(aBuilder, InitiativeOrderChange(List(itb)))

      snap.initiativeTrackerFromID(ioa0) must throwA[NoSuchElementException]
      snap.getInitiativeOrder must_== List(iob)
    }

    "store combatants on a RosterChange" in new emptyContext {
      val snap = processChanges(aBuilder,
        RosterChange(Map(
          combA -> generateCombatantAspectSet(combA),
          combB -> generateCombatantAspectSet(combB))))

      for (comb <- List(combA, combB)) {
        val cv = snap.combatantViewFromID(comb)
        cv must not beNull;
        cv.definition must_== generateCombatantRosterDefinition(comb)
        cv.definition.cid must_== comb
        cv.healthTracker must_== baseHealth
        cv.effects must_== generateEffects(comb)
        cv.comment must_== "dont"
      }
    }
  }

  trait loadedContext extends Scope {
    val aBuilder = new CombatStateSnapshotBuilder()
    processChanges(aBuilder,
      RosterChange(Map(
        combA -> generateCombatantAspectSet(combA),
        combB -> generateCombatantAspectSet(combB))),
      InitiativeOrderChange(List(ita0, itb)))
  }

  "a builder with loaded context" should {
    "throw exception if not in roster" in new loadedContext {
      processChanges(aBuilder, CombatantChange(CombatantID("C"), CombatantComment("do it"))) must throwA[NoSuchElementException]
    }

    "update combatant comment" in new loadedContext {
      val snap = processChanges(aBuilder, CombatantChange(combA, CombatantComment("do it")))
      snap.combatantViewFromID(combA).comment must_== "do it"
    }

    "update health" in new loadedContext {
      val snap = processChanges(aBuilder, CombatantChange(combA, modHealth))
      snap.combatantViewFromID(combA).healthTracker must_== modHealth
    }

    "update effects" in new loadedContext {
      val nel = EffectList(combA, Nil).addEffect(combA, null, null)
      val snap = processChanges(aBuilder, CombatantChange(combA, nel))
      snap.combatantViewFromID(combA).effects must_== nel
    }

    "update definition" in new loadedContext {
      val nDef = CombatantRosterDefinition(combA, "alias", null)
      val snap = processChanges(aBuilder, CombatantChange(combA, nDef))
      snap.combatantViewFromID(combA).definition.alias must_== "alias"
    }

    "change roster head on InitiativeOrderFirstChange" in new loadedContext {
      val snap = processChanges(aBuilder, InitiativeOrderFirstChange(iob))
      snap.nextUp must_== Some(iob)
    }

    "change roster head to None on InitiativeOrderFirstChange(null)" in new loadedContext {
      val snap = processChanges(aBuilder, InitiativeOrderFirstChange(null))
      snap.nextUp must_== None
    }

    "throw exception if InitiativeOrderFirstChange points to something wrong" in new loadedContext {
      val ioc = InitiativeOrderID(CombatantID("C"), 0)
      processChanges(aBuilder, InitiativeOrderFirstChange(ioc)) must throwA[NoSuchElementException]
    }

    "update comment and combat state on CombatMetaDataChange" in new loadedContext {
      val snap = processChanges(aBuilder, CombatMetaDataChange(true, "fumes are active"))
      snap.combatComment must_== "fumes are active"
      snap.isCombatStarted must beTrue
    }

    "before a change to start combat, must not be in combat" in new loadedContext {
      aBuilder.getSnapshot().isCombatStarted must beFalse
    }
  }
}