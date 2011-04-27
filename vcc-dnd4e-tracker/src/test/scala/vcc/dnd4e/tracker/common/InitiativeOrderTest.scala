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
//$Id$
package vcc.dnd4e.tracker.common

import org.specs.SpecificationWithJUnit
import vcc.dnd4e.domain.tracker.common.{InitiativeTracker, InitiativeDefinition}

class InitiativeOrderTest extends SpecificationWithJUnit with CommonCombatantID {

  private var order: InitiativeOrder = null

  val emptyOrder = beforeContext {
    order = InitiativeOrder.empty()
  }

  val loadedOrder = beforeContext {
    order = InitiativeOrder.empty()
    order = order.setInitiative(InitiativeDefinition(combA, 10, List(14, 9)))
    order = order.setInitiative(InitiativeDefinition(combB, 10, List(10)))
    order = order.setInitiative(InitiativeDefinition(combC, 8, List(18)))
    order = order.setInitiative(InitiativeDefinition(combD, 4, List(7)))
  }

  "an Empty initaitive order" ->- (emptyOrder) should {
    "not allow the start of combat" in {
      order.startCombat() must throwAn(new IllegalStateException("No combatant has initiative"))
    }

    "not have a nextup" in {
      order.nextUp must_== None
    }

    "not allow the start of combat" in {
      order.rotate() must throwAn(new IllegalStateException("Rotate not allowed before start of combat"))
    }

    "add a simple InitiativeDefinition to the order" in {
      val nOrder = order.setInitiative(InitiativeDefinition(combA, 10, List(3)))
      nOrder.order must_== List(ioA0)
      nOrder.tracker must beDefinedAt(ioA0)
      nOrder.tracker(ioA0) must_== InitiativeTracker(ioA0, 0, 3, InitiativeTracker.state.Waiting)
      nOrder.baseList mustExist (ir => ir.uniqueId == ioA0)
      nOrder.baseList.length must_== 1
    }

    "add a complex InitiativeDefinition to order" in {
      val nOrder = order.setInitiative(InitiativeDefinition(combA, 10, List(3, 13)))
      nOrder.order must_== List(ioA1, ioA0)
      nOrder.tracker must beDefinedAt(ioA1, ioA0)
      nOrder.tracker(ioA0) must_== InitiativeTracker(ioA0, 0, 3, InitiativeTracker.state.Waiting)
      nOrder.tracker(ioA1) must_== InitiativeTracker(ioA1, 0, 13, InitiativeTracker.state.Waiting)
      nOrder.baseList.map(ir => ir.uniqueId) must_== List(ioA1, ioA0)
      nOrder.baseList.length must_== 2
    }

    "add two InitiativeDefinitions to the order" in {
      val iOrder = order.setInitiative(InitiativeDefinition(combA, 10, List(3, 13)))
      val nOrder = iOrder.setInitiative(InitiativeDefinition(combB, 5, List(9)))
      nOrder.order must_== List(ioA1, ioB0, ioA0)
      nOrder.tracker must beDefinedAt(ioA1, ioA0, ioB0)
      nOrder.baseList.map(ir => ir.uniqueId) must_== List(ioA1, ioB0, ioA0)
      nOrder.baseList.length must_== 3
    }

    "preserve ordering of first insert when adding tied initiatives" in {
      val iDef1 = InitiativeDefinition(combA, 10, List(14, 14, 14, 14, 14))
      val iDef2 = InitiativeDefinition(combB, 10, List(14, 14, 14, 14, 14))
      val o1 = order.setInitiative(iDef1)
      val o2 = o1.setInitiative(iDef2)
      o2.baseList.filter(x => x.uniqueId.combId == combA).map(x => x.uniqueId) must_== o1.order
    }

    "be valid" in {
      order.isValid must beTrue
    }
  }

  "a loaded InitativeOrder" ->- (loadedOrder) should {
    "set to first on combat start" in {
      val nOrder = order.startCombat()
      nOrder.nextUp must_== Some(ioC0)
    }
    "rotate do next" in {
      val nOrder = order.startCombat.rotate
      nOrder.nextUp must_== Some(order.order(1))
    }

    "rotate to first once we have rotated through the entire order" in {
      var nOrder = order.startCombat
      for (x <- 1 to order.order.length - 1) {
        nOrder = nOrder.rotate()
        nOrder.nextUp must_== Some(order.order(x))
      }
      nOrder = nOrder.rotate
      nOrder.nextUp must_== Some(order.order.head)
    }

    "not change when updating order" in {
      val nOrder = order.startCombat.setInitiative(InitiativeDefinition(combE, 15, List(20)))
      nOrder.order.head must_== ioE0
      nOrder.nextUp must_== order.startCombat.nextUp
    }

    "remove all result and initiative information after endCombat is called" in {
      val sOrder = order.startCombat().rotate
      val nOrder = sOrder.endCombat
      nOrder must_== InitiativeOrder.empty
    }
    "moveBefore throws exception if combat not start" in {
      order.moveBefore(ioA0, ioC0) must throwA(new IllegalStateException("Can't move if combat not started"))
    }
    "moveBefore stores a change and updates other if valid" in {
      val nOrder = order.startCombat.moveBefore(ioB0, ioC0)
      nOrder.order must_== List(ioB0, ioC0, ioA0, ioA1, ioD0)
      nOrder.reorderList must_== List((ioB0, ioC0))
    }

    "remove combat throws exception if combat started" in {
      order.startCombat.removeCombatant(combA) must throwA(new IllegalStateException("Can't remove after combat start"))
    }

    "removes a single entry from the combat?" in {
      val nOrder = order.removeCombatant(combA)
      nOrder.tracker mustNot contain(ioA0)
      nOrder.tracker mustNot contain(ioA1)
      nOrder.order mustNot contain(ioA0)
      nOrder.order mustNot contain(ioA1)
    }

    "have true isValid if each IOI has it's tracker" in {
      order.isValid() must beTrue
    }

    "not be valid if IT tracker is missing for some IOI" in {
      order.copy(order = ioE0 :: order.order).isValid must beFalse
    }

    "not be valid if reorder list has elements not in the order" in {
      order.copy(reorderList = List((ioE0, ioA0))).isValid must beFalse
      order.copy(reorderList = List((ioA0, ioE0))).isValid must beFalse
    }

    "no be valid if there is IT for some IOI not in order" in {
      order.copy(tracker = order.tracker.updated(ioE0, InitiativeTracker(ioE0, 0, 20, InitiativeTracker.state.Waiting))).isValid must beFalse
    }

  }

}
