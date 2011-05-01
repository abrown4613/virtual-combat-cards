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
package vcc.dnd4e.domain.tracker.transactional


import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}

import vcc.dnd4e.tracker.common._
import vcc.dnd4e.domain.tracker.common.Command._
import collection.mutable.Queue
import vcc.controller.message.TransactionalAction
import org.specs.mock.Mockito
import vcc.controller.transaction.Transaction
import vcc.controller.{UnhandledActionException, CommandSource}
import vcc.dnd4e.domain.tracker.common.CombatStateRules

@RunWith(classOf[JUnitSuiteRunner])
class HealthActionHandlerTest extends JUnit4(HealthActionHandlerSpec)

trait MockCombatContextSpecification extends Specification with Mockito {
  var mOrder: InitiativeOrder = null
  var mRoster: CombatantRoster = null
  var mMeta: CombatMetaData = null
  var mRule: CombatStateRules = null
  var mSource: CommandSource = null
  var rState: CombatState = null
  var aController: AbstractCombatController = null

  def setupMockContext() {
    mOrder = mock[InitiativeOrder]
    mRoster = mock[CombatantRoster]
    mMeta = mock[CombatMetaData]
    mRule = mock[CombatStateRules]
    mSource = mock[CommandSource]
    rState = new CombatState(mOrder, mRoster, mMeta)
  }

}

object HealthActionHandlerSpec extends MockCombatContextSpecification {

  class PartialCombatController(rules: CombatStateRules, state: CombatState, queue: Queue[TransactionalAction])
    extends AbstractCombatController(rules, state, queue)
    with HealthActionHandler

  var trans: Transaction = null

  val mockedContext = beforeContext {
    setupMockContext()
    aController = new PartialCombatController(mRule, rState, new Queue[TransactionalAction])
    trans = new Transaction()
  }

  val combA = CombatantID("A")
  val combB = CombatantID("B")

  /**
   * Returns a combatant, base HealthTracker and a modified HealthTracker.
   * Base HealthTracker.applyDamage(99) returns a tracker with dead status
   * Base HealthTracker.applyDamage(10) returns a tracker that is not dead
   */
  def mockCombatantWithHealth(comb: CombatantID): (Combatant, HealthTracker, HealthTracker) = {
    val c = mock[Combatant]
    val h = mock[HealthTracker]
    val hm = mock[HealthTracker]
    val dead = mock[HealthTracker]

    dead.status() returns HealthTracker.Status.Dead
    hm.status() returns HealthTracker.Status.Bloody
    h.applyDamage(10) returns hm
    h.applyDamage(99) returns dead
    h.failDeathSave() returns hm
    h.raiseFromDead() returns hm
    h.setTemporaryHitPoints(8, false) returns hm
    h.heal(12) returns hm
    h.rest(true) returns hm
    h.rest(false) returns hm
    c.health returns h
    mRoster.isDefinedAt(comb) returns true
    mRoster.combatant(comb) returns c
    (c, h, hm) // Return the mock, and base and modified HealthTracker
  }

  "a HealthActionHandler" ->- (mockedContext) should {
    "not handle actions to unexisting CombatantID" in {
      val messages: List[TransactionalAction] = List(
        ApplyDamage(combA, 10), SetTemporaryHP(combA, 10), FailDeathSave(combA),
        HealDamage(combA, 10), RevertDeath(combA), SetComment(combA, "fiat lux!"))

      mRoster.isDefinedAt(combA) returns false

      for (msg <- messages) {
        aController.dispatch(trans, mSource, msg) must throwAn[UnhandledActionException]
      }
      there was atLeast(6)(mRoster).isDefinedAt(combA)
    }

    "apply damage to target" in {
      val (comb, health, modHealth) = mockCombatantWithHealth(combA)

      aController.dispatch(trans, mSource, ApplyDamage(combA, 10))

      there was one(health).applyDamage(10)
      there was one(modHealth).status
      there was one(comb).health_=(modHealth)(trans)
    }

    "check if all acting are dead when a combatant is dies" in {
      val (comb, health, modHealth) = mockCombatantWithHealth(combA)
      mRule.areAllCombatantInOrderDead(rState) returns false

      aController.dispatch(trans, mSource, ApplyDamage(combA, 99))

      there was one(health).applyDamage(99)
      there was one(mRule).areAllCombatantInOrderDead(rState)
    }


    "stop combat if all acting are dead when the last combatant dies" in {
      val (comb, health, modHealth) = mockCombatantWithHealth(combA)
      mRule.areAllCombatantInOrderDead(rState) returns true

      aController.dispatch(trans, mSource, ApplyDamage(combA, 99))

      there was one(mRule).areAllCombatantInOrderDead(rState)
      there was one(mMeta).endCombat()(trans)
      there was one(mOrder).clearOrder()(trans)
    }

    "set temporary hit points" in {
      val (comb, health, modHealth) = mockCombatantWithHealth(combA)

      aController.dispatch(trans, mSource, SetTemporaryHP(combA, 8))

      there was one(health).setTemporaryHitPoints(8, false)
      there was one(comb).health_=(modHealth)(trans)
    }

    "apply healing to combatant" in {
      val (comb, health, modHealth) = mockCombatantWithHealth(combA)

      aController.dispatch(trans, mSource, HealDamage(combA, 12))

      there was one(health).heal(12)
      there was one(comb).health_=(modHealth)(trans)
    }

    "update tracker for failed death saves" in {
      val (comb, health, modHealth) = mockCombatantWithHealth(combA)

      aController.dispatch(trans, mSource, FailDeathSave(combA))

      there was one(health).failDeathSave()
      there was one(comb).health_=(modHealth)(trans)
    }

    "revert death if asked" in {
      val (comb, health, modHealth) = mockCombatantWithHealth(combA)

      aController.dispatch(trans, mSource, RevertDeath(combA))

      there was one(health).raiseFromDead()
      there was one(comb).health_=(modHealth)(trans)
    }

    "update all combatant for short rest" in {
      //This involves clearing failed death save
      val lc = List[(Combatant, HealthTracker, HealthTracker)](mockCombatantWithHealth(combA), mockCombatantWithHealth(combB))
      mRoster.allCombatantIDs returns List(combA, combB)

      aController.dispatch(trans, mSource, ApplyRest(false))

      there was one(mRoster).allCombatantIDs
      for ((comb, health, modHealth) <- lc) {
        there was one(health).rest(false)
        there was one(comb).health_=(modHealth)(trans)
      }
    }

    "update all combatant for rest" in {
      //Recover to full health
      val lc = List[(Combatant, HealthTracker, HealthTracker)](mockCombatantWithHealth(combA), mockCombatantWithHealth(combB))
      mRoster.allCombatantIDs returns List(combA, combB)

      aController.dispatch(trans, mSource, ApplyRest(true))

      there was one(mRoster).allCombatantIDs
      for ((comb, health, modHealth) <- lc) {
        there was one(health).rest(true)
        there was one(comb).health_=(modHealth)(trans)
      }
    }
  }

  "aController doing some help to the lonely comment field" ->- (mockedContext) should {

    "set comment " in {
      val mComb = mock[Combatant]
      mRoster.isDefinedAt(combA) returns true
      mRoster.combatant(combA) returns mComb

      aController.dispatch(trans, mSource, SetComment(combA, "panic!"))

      there was one(mComb).comment_=("panic!")(trans)
    }

  }


}