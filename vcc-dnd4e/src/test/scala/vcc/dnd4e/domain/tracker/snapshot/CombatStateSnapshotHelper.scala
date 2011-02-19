/**
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
package vcc.dnd4e.domain.tracker.snapshot

import vcc.dnd4e.domain.tracker.common._
import vcc.controller.transaction.ChangeNotification
import vcc.controller.SnapshotBuilder

trait CombatStateSnapshotHelper[S] {
  // Data
  val combA = CombatantID("A")
  val combB = CombatantID("B")
  val combC = CombatantID("C")
  val ioa0 = InitiativeOrderID(combA, 0)
  val ioa1 = InitiativeOrderID(combA, 1)
  val iob = InitiativeOrderID(combB, 0)

  val ita0 = InitiativeTracker(ioa0, 1, 0, InitiativeTracker.state.Waiting)
  val ita0m = InitiativeTracker(ioa0, 2, 0, InitiativeTracker.state.Acting)
  val itb = InitiativeTracker(iob, 1, 0, InitiativeTracker.state.Waiting)

  val baseHealth = HealthTracker(10, 0, 0, null)
  val modHealth = HealthTracker(12, 0, 0, null)

  def processChanges(builder: SnapshotBuilder[S], changes: ChangeNotification*): S = {
    builder.beginChanges()
    changes.foreach(builder.processChange(_))
    builder.endChanges()
    builder.getSnapshot()
  }

  def generateCombatantRosterDefinition(comb: CombatantID) = CombatantRosterDefinition(comb, null, null)

  def generateEffects(comb: CombatantID) = EffectList(comb, Nil)

  def generateCombatantAspectSet(comb: CombatantID): Set[CombatantAspect] = {
    Set(generateCombatantRosterDefinition(comb),
      baseHealth,
      CombatantComment("dont"),
      generateEffects(comb))
  }

}