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
package vcc.dnd4e.view.helper

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import vcc.dnd4e.view.UnifiedSequenceTable
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.domain.tracker.common.{CombatantStateView, CombatStateView}

class UnifiedCombatantArrayBuilderTest extends SpecificationWithJUnit with Mockito {

  private val combA = CombatantID("A")
  private val combB = CombatantID("B")
  private val combC = CombatantID("C")
  private val ioa0 = InitiativeOrderID(combA, 0)
  private val ita0 = InitiativeTracker(ioa0, 1, 0, InitiativeState.Waiting)

  private val mockCombatant = Map(combA -> mock[CombatantStateView], combB -> mock[CombatantStateView], combC -> mock[CombatantStateView])
  private val combatState = mock[CombatStateView]

  combatState.getInitiativeOrder returns List(ioa0)
  combatState.initiativeTrackerFromID(ioa0) returns ita0
  combatState.allCombatantIDs returns mockCombatant.keys.toList
  for ((k, v) <- mockCombatant) {
    combatState.combatantViewFromID(k) returns v
  }

  "the UnifiedCombatantArrayBuilder" should {
    "call reserve builder with CombatantID that are not in order" in {
      val mOrderBuilder = mock[InitiativeOrderViewBuilder]
      val mReserve = mock[ReserveViewBuilder]
      mOrderBuilder.buildOrder(any[CombatStateView]) answers {
        cs => cs.asInstanceOf[CombatStateView].getInitiativeOrder
      }
      mReserve.buildReserve(any[CombatStateView]) returns Seq()
      UnifiedSequenceTable.buildList(combatState, mOrderBuilder, mReserve)
      there was one(mReserve).buildReserve(combatState)
    }

    "call order builder with item in the initative order" in {
      val mOrderBuilder = mock[InitiativeOrderViewBuilder]
      val mReserve = mock[ReserveViewBuilder]
      mOrderBuilder.buildOrder(any[CombatStateView]) returns Seq(ioa0)
      mReserve.buildReserve(any[CombatStateView]) returns Seq(combC, combB)

      UnifiedSequenceTable.buildList(combatState, mOrderBuilder, mReserve)
      there was one(mOrderBuilder).buildOrder(combatState)
    }

    "build an order with what was returned" in {
      val mOrderBuilder = mock[InitiativeOrderViewBuilder]
      val mReserve = mock[ReserveViewBuilder]
      mOrderBuilder.buildOrder(any[CombatStateView]) returns Seq(ioa0)
      mReserve.buildReserve(any[CombatStateView]) returns Seq(combC, combB)

      val ret = UnifiedSequenceTable.buildList(combatState, mOrderBuilder, mReserve)
      ret.elements.length must_== 3
      ret(0).initiative must_== ita0
      ret(1).initiative must_== null
      ret(1).combId must_== combC
      ret(2).combId must_== combB
    }
  }
}