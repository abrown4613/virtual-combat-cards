/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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

import org.specs2.SpecificationWithJUnit
import org.specs2.mock.Mockito

class UnifiedSequenceTableTest extends SpecificationWithJUnit {

  private val combA = CombatantID("A")
  private val combB = CombatantID("B")
  private val combC = CombatantID("C")
  private val combD = CombatantID("D")

  private val ioa0 = InitiativeOrderID(combA, 0)
  private val ioa1 = InitiativeOrderID(combA, 1)
  private val iod0 = InitiativeOrderID(combD, 0)

  private val ita0 = InitiativeTracker(ioa0, 1, 0, InitiativeState.Acting)
  private val ita1 = InitiativeTracker(ioa1, 1, 0, InitiativeState.Waiting)
  private val itd0 = InitiativeTracker(iod0, 1, 0, InitiativeState.Waiting)

  private val uci_A0 = UnifiedCombatantID(combA, ioa0)
  private val uci_D0 = UnifiedCombatantID(combD, iod0)
  private val uci_A1 = UnifiedCombatantID(combA, ioa1)
  private val uci_B = UnifiedCombatantID(combB, null)
  private val uci_C = UnifiedCombatantID(combC, null)

  private val livingHealthTracker = HealthTracker.createTracker(MinionHealthDefinition)

  def is =
    "UnifiedSequenceTable.Builder" ^
      "build default robin view" ! withMockView().buildDefaultRobinView ^
      "build direct order view" ! withMockView().buildDirectOrderView ^
      "build robin order view after setting direct" ! withMockView().buildDirectOrderViewAgain ^
      "hide dead combatants" ! withMockView().buildHidingNonActionDead ^
      "show acting even if dead when hiding dead combatants" ! withMockView().buildHidingActingDead ^
      "show dead if request" ! withMockView().buildShowDead ^
      endp ^
      "utility" ^
      "  find index if in table" ! withMockView().findExistentIndex ^
      "  not find index if not in table" ! withMockView().findNonExistentIndex ^
      endp ^
      end

  case class withMockView() extends Mockito {
    private val mockCombatant = buildCombatantView(combA, combB, combC, combD)
    private val combatState = mock[CombatStateView]
    private val builder = new UnifiedSequenceTable.Builder

    setup()

    def buildDirectOrderView = {
      builder.useDirectOrder()
      val table = builder.build(combatState)
      listUnifiedCombatantID(table) must_== List(uci_A1, uci_A0, uci_D0, uci_B, uci_C)
    }

    def buildDefaultRobinView = {
      val table = builder.build(combatState)
      (table.state must_== combatState) and
        (listUnifiedCombatantID(table) must_== List(uci_A0, uci_D0, uci_A1, uci_B, uci_C))
    }

    def buildDirectOrderViewAgain = {
      builder.useDirectOrder()
      builder.useRobinOrder()
      listUnifiedCombatantID(builder.build(combatState)) must_== List(uci_A0, uci_D0, uci_A1, uci_B, uci_C)
    }

    def buildHidingNonActionDead = {
      killCombatant(mockCombatant(combD))
      killCombatant(mockCombatant(combC))
      builder.hideDead()
      listUnifiedCombatantID(builder.build(combatState)) must_== List(uci_A0, uci_A1, uci_B)
    }

    def buildHidingActingDead = {
      killCombatant(mockCombatant(combA))
      killCombatant(mockCombatant(combC))
      builder.useDirectOrder()
      builder.hideDead()
      listUnifiedCombatantID(builder.build(combatState)) must_== List(uci_A0, uci_D0, uci_B)
    }

    def buildShowDead = {
      killCombatant(mockCombatant(combD))
      builder.hideDead()
      builder.showDead()
      listUnifiedCombatantID(builder.build(combatState)) must_== List(uci_A0, uci_D0, uci_A1, uci_B, uci_C)
    }

    def findExistentIndex = {
      killCombatant(mockCombatant(combD))
      builder.build(combatState).indexOf(uci_D0) must_== Some(1)
    }

    def findNonExistentIndex = {
      killCombatant(mockCombatant(combD))
      builder.hideDead()
      builder.build(combatState).indexOf(uci_D0) must_== None
    }

    private def killCombatant(combatantStateView: CombatantStateView) {
      val dead = HealthTracker.createTracker(MinionHealthDefinition).applyDamage(1)
      combatantStateView.health returns dead
    }

    private def listUnifiedCombatantID(table: UnifiedSequenceTable): List[UnifiedCombatantID] = {
      table.elements.map(_.unifiedId).toList
    }

    private def setup() {
      combatState.getInitiativeOrder returns List(ioa1, ioa0, iod0)
      combatState.initiativeTrackerFromID(null).throws(new NoSuchElementException())
      combatState.initiativeTrackerFromID(ioa0) returns ita0
      combatState.initiativeTrackerFromID(ioa1) returns ita1
      combatState.initiativeTrackerFromID(iod0) returns itd0
      combatState.allCombatantIDs returns mockCombatant.keys.toList
      for ((k, v) <- mockCombatant) {
        combatState.combatantViewFromID(k) returns v
      }
      combatState.nextUp returns Some(ioa0)
    }

    private def buildCombatantView(ids: CombatantID*): Map[CombatantID, CombatantStateView] = {
      val pairs: Seq[(CombatantID, CombatantStateView)] = for (id <- ids) yield {
        val m = mock[CombatantStateView]
        m.health returns livingHealthTracker
        (id -> m)
      }
      pairs.toMap
    }
  }
}