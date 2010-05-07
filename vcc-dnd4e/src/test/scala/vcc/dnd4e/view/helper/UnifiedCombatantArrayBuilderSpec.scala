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
package vcc.dnd4e.view.helper


import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import org.specs.mock.Mockito
import vcc.dnd4e.domain.tracker.snapshot.{CombatState, CombatStateSnapshotHelper}
import vcc.dnd4e.domain.tracker.common.CombatantStateView
import vcc.dnd4e.view.UnifiedSequenceTable

@RunWith(classOf[JUnitSuiteRunner])
class UnifiedCombatantArrayBuilderTest extends JUnit4(UnifiedCombatantArrayBuilderSpec)

object UnifiedCombatantArrayBuilderSpec extends Specification with Mockito with CombatStateSnapshotHelper[String] {
  val cs = CombatState(
    false,
    null,
    List(ioa0), Map(ioa0 -> ita0),
    Some(ioa0),
    Map(combA -> mock[CombatantStateView], combB -> mock[CombatantStateView], combC -> mock[CombatantStateView]))


  "the UnifiedCombatantArrayBuilder" should {
    "call reserve builder with CombatantID that are not in order" in {
      val mOrderBuilder = mock[InitiativeOrderViewBuilder]
      val mReserve = mock[ReserveViewBuilder]
      mOrderBuilder.buildOrder(any[CombatState]) answers {cs => cs.asInstanceOf[CombatState].order}
      mReserve.buildReserve(any[CombatState]) answers {cs => cs.asInstanceOf[CombatState].combatantsNotInOrder().toSeq}

      UnifiedSequenceTable.buildList(cs, mOrderBuilder, mReserve)
      there was one(mReserve).buildReserve(cs)
    }
    "call order builder with item in the initative order" in {
      val mOrderBuilder = mock[InitiativeOrderViewBuilder]
      val mReserve = mock[ReserveViewBuilder]
      mOrderBuilder.buildOrder(any[CombatState]) returns Seq(ioa0)
      mReserve.buildReserve(any[CombatState]) returns Seq(combC, combB)

      UnifiedSequenceTable.buildList(cs, mOrderBuilder, mReserve)
      there was one(mOrderBuilder).buildOrder(cs)
    }

    "build an order with what was returned" in {
      val mOrderBuilder = mock[InitiativeOrderViewBuilder]
      val mReserve = mock[ReserveViewBuilder]
      mOrderBuilder.buildOrder(any[CombatState]) returns Seq(ioa0)
      mReserve.buildReserve(any[CombatState]) returns Seq(combC, combB)

      val ret = UnifiedSequenceTable.buildList(cs, mOrderBuilder, mReserve)
      ret.elements.length must_== 3
      ret(0).initiative must_== ita0
      ret(1).initiative must beNull
      ret(1).combId must_== combC
      ret(2).combId must_== combB
    }
  }

}