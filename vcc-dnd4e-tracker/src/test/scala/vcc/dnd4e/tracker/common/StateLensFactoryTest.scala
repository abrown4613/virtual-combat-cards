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
package vcc.dnd4e.tracker.common

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import vcc.dnd4e.tracker.StateLensFactory
import vcc.scalaz.Lens

class StateLensFactoryTest extends SpecificationWithJUnit with Mockito with DemoCompendium {
  val lf = mock[StateLensFactory]
  val mhl = mockLens[CombatState, HealthTracker]()
  val ms = mock[CombatState]
  val msAfter = mock[CombatState]

  def mockLens[S, T](): Lens[S, T] = Lens[S, T](mock[S => T], mock[(S, T) => S])

  "a StateLensFactory" should {

    "be mockable" in {
      lf.combatantHealth(CombatantID("A")) returns mhl
      val ht1 = HealthTracker.createTracker(CombatantType.Character, 40)

      mhl.get(ms) returns ht1

      mhl.set(ms, ht1) returns msAfter

      val l = lf.combatantHealth(CombatantID("A"))
      (l must be equalTo(mhl))
      val ms2 = l.set(ms, l.get(ms))
      ms2 must_== msAfter
    }

    "allow the addition of new combatant" in {
      val combDef = CombatantRosterDefinition(CombatantID("A"), null, goblinDefinition)
      val s = CombatState.empty
      val s2 = StateLensFactory.combatant(CombatantID("A")).set(s, Combatant(combDef))
      s2.roster.isDefinedAt(CombatantID("A")) must beTrue
      val l = StateLensFactory.combatant(CombatantID("A"))
      l.get(s2) must_== Combatant(combDef)
      l.get(s) must throwA[NoSuchElementException]
    }
  }
}
