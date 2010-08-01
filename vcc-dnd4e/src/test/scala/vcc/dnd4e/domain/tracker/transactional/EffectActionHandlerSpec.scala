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
import vcc.controller.message.TransactionalAction
import collection.mutable.Queue
import org.specs.mock.Mockito
import org.mockito.Matchers._
import vcc.dnd4e.domain.tracker.common._
import Command._
import vcc.controller.transaction.Transaction
import vcc.controller.{UnhandledActionException, CommandSource}

@RunWith(classOf[JUnitSuiteRunner])
class EffectActionHandlerTest extends JUnit4(EffectActionHandlerSpec)

object EffectActionHandlerSpec extends Specification with Mockito {
  class PartialCombatController(rules: CombatStateRules, state: CombatState, queue: Queue[TransactionalAction])
          extends AbstractCombatController(rules, state, queue)
                  with EffectActionHandler

  var mOrder: InitiativeOrder = null
  var mRoster: CombatantRoster = null
  var mMeta: CombatMetaData = null
  var mRule: CombatStateRules = null
  var mSource: CommandSource = null
  var rState: CombatState = null
  var aController: AbstractCombatController = null

  val mockedContext = beforeContext {
    mOrder = mock[InitiativeOrder]
    mRoster = mock[CombatantRoster]
    mMeta = mock[CombatMetaData]
    mRule = mock[CombatStateRules]
    mSource = mock[CommandSource]
    rState = new CombatState(mOrder, mRoster, mMeta)
    aController = new PartialCombatController(mRule, rState, new Queue[TransactionalAction])
  }

  val combA = CombatantID("A")
  val combB = CombatantID("B")
  val effA1 = EffectID(combA, 1)
  val ioa = InitiativeOrderID(combA, 0)

  val trans = new Transaction()


  def mockCombatantEffectList(comb: CombatantID): (Combatant, EffectList, EffectList) = {
    val mComb = mock[Combatant]
    val mEffList = mock[EffectList]
    val mEffList2 = mock[EffectList]
    mRoster.isDefinedAt(comb) returns true
    mRoster.combatant(comb) returns mComb
    mComb.effects returns mEffList
    mComb.definition returns CombatantRosterDefinition(comb, null, null)
    mEffList.transformAndFilter(any[EffectTransformation]) returns mEffList2
    mEffList.addEffect(any[CombatantID], any[Condition], any[Duration]) returns mEffList2
    (mComb, mEffList, mEffList2)
  }

  "aController handling a SustainEffect" ->- (mockedContext) should {
    "not handle actions to unexisting combatant" in {
      mRoster.isDefinedAt(combA) returns false
      aController.dispatch(trans, mSource, SustainEffect(effA1)) must throwAn[UnhandledActionException]
      there was one(mRoster).isDefinedAt(combA)
    }

    "handle actions to existing combatant" in {
      val (comb, effList, effList2) = mockCombatantEffectList(combA)
      aController.dispatch(trans, mSource, SustainEffect(effA1))
      there was one(effList).transformAndFilter(EffectTransformation.sustainEffect(effA1))
      there was one(comb).effects_=(effList2)(trans)
    }
  }

  "aController handling a CancelEffect" ->- (mockedContext) should {
    "not handle actions to unexisting combatant" in {
      mRoster.isDefinedAt(combA) returns false
      aController.dispatch(trans, mSource, CancelEffect(effA1)) must throwAn[UnhandledActionException]
      there was one(mRoster).isDefinedAt(combA)
    }

    "handle actions to existing combatant" in {
      val (comb, effList, effList2) = mockCombatantEffectList(combA)
      aController.dispatch(trans, mSource, CancelEffect(effA1))
      there was one(effList).transformAndFilter(EffectTransformation.cancelEffect(effA1))
      there was one(comb).effects_=(effList2)(trans)
    }
  }

  "aController handling a UpdateEffectCondition" ->- (mockedContext) should {

    val newCondition = Effect.Condition.Generic("anything", false)

    "not handle actions to unexisting combatant" in {
      mRoster.isDefinedAt(combA) returns false
      aController.dispatch(trans, mSource, UpdateEffectCondition(effA1, newCondition)) must throwAn[UnhandledActionException]
      there was one(mRoster).isDefinedAt(combA)
    }

    "handle actions to existing combatant" in {
      val (comb, effList, effList2) = mockCombatantEffectList(combA)
      aController.dispatch(trans, mSource, UpdateEffectCondition(effA1, newCondition))
      there was one(effList).transformAndFilter(EffectTransformation.updateCondition(effA1, newCondition))
      there was one(comb).effects_=(effList2)(trans)
    }
  }

  "aController handling a AddEffect" ->- (mockedContext) should {

    val newCondition = Effect.Condition.Generic("anything", false)
    val duration = Duration.SaveEnd

    "not handle actions to unexisting combatant" in {
      mRoster.isDefinedAt(combA) returns false
      aController.dispatch(trans, mSource, AddEffect(combA, combB, newCondition, duration)) must throwAn[UnhandledActionException]
      there was one(mRoster).isDefinedAt(combA)
    }

    "handle actions to existing combatant" in {
      val (comb, effList, effList2) = mockCombatantEffectList(combA)
      aController.dispatch(trans, mSource, AddEffect(combA, combB, newCondition, duration))
      there was one(effList).addEffect(combB, newCondition, duration)
      there was one(comb).effects_=(effList2)(trans)
    }
  }

  "aController handling a InitiativeAction" ->- (mockedContext) should {

    "apply changes for StartRound on all element" in {
      // We test this against two trackers
      val lc = List[(Combatant, EffectList, EffectList)](mockCombatantEffectList(combA), mockCombatantEffectList(combB))
      mRoster.allCombatantIDs returns List(combA, combB)

      aController.dispatch(trans, mSource, InternalInitiativeAction(ioa, InitiativeTracker.action.StartRound))

      there was one(mRoster).allCombatantIDs
      for ((comb, effList, effList2) <- lc) {
        there was one(effList).transformAndFilter(EffectTransformation.startRound(ioa))
        there was one(comb).effects_=(effList2)(trans)
      }
    }

    "apply changes for EndRound on all element" in {
      // We test this against two trackers
      val lc = List[(Combatant, EffectList, EffectList)](mockCombatantEffectList(combA), mockCombatantEffectList(combB))
      mRoster.allCombatantIDs returns List(combA, combB)

      aController.dispatch(trans, mSource, InternalInitiativeAction(ioa, InitiativeTracker.action.EndRound))

      there was one(mRoster).allCombatantIDs
      for ((comb, effList, effList2) <- lc) {
        there was one(effList).transformAndFilter(EffectTransformation.endRound(ioa))
        there was one(comb).effects_=(effList2)(trans)
      }
    }

    "apply changes for Delay on all element" in {
      // We test this against two trackers
      val lc = List[(Combatant, EffectList, EffectList)](mockCombatantEffectList(combA), mockCombatantEffectList(combB))
      mRoster.allCombatantIDs returns List(combA, combB)

      mRule.areAllied(rState, combA, combB) returns false
      mRule.areAllied(rState, combA, combA) returns true

      aController.dispatch(trans, mSource, InternalInitiativeAction(ioa, InitiativeTracker.action.Delay))

      there was one(mRoster).allCombatantIDs
      for ((comb, effList, effList2) <- lc) {
        val cid = comb.definition.cid
        val ally = (cid == combA)
        there was one(mRule).areAllied(rState, combA, cid)
        there was one(effList).transformAndFilter(EffectTransformation.processDelay(ally, ioa))
        there was one(comb).effects_=(effList2)(trans)
      }
    }

    "apply changes for Rest normal to all element" in {
      // We test this against two trackers
      val lc = List[(Combatant, EffectList, EffectList)](mockCombatantEffectList(combA), mockCombatantEffectList(combB))
      mRoster.allCombatantIDs returns List(combA, combB)

      aController.dispatch(trans, mSource, ApplyRest(false))

      there was one(mRoster).allCombatantIDs
      for ((comb, effList, effList2) <- lc) {
        there was one(effList).transformAndFilter(EffectTransformation.applyRest)
        there was one(comb).effects_=(effList2)(trans)
      }
    }

    "apply changes for Rest Extended to all element" in {
      // We test this against two trackers
      val lc = List[(Combatant, EffectList, EffectList)](mockCombatantEffectList(combA), mockCombatantEffectList(combB))
      mRoster.allCombatantIDs returns List(combA, combB)

      aController.dispatch(trans, mSource, ApplyRest(true))

      there was one(mRoster).allCombatantIDs
      for ((comb, effList, effList2) <- lc) {
        there was one(effList).transformAndFilter(EffectTransformation.applyRest)
        there was one(comb).effects_=(effList2)(trans)
      }
    }


  }
}