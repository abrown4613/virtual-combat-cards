/**
 * Copyright (C) 2008-2010 tms - Thomas Santana <tms@exnebula.org>
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
import org.specs.mock.Mockito
import vcc.controller.transaction.Transaction
import vcc.controller.{IllegalActionException, CommandSource}
import vcc.dnd4e.domain.tracker.common.Command._
import vcc.infra.datastore.naming.EntityID
import vcc.dnd4e.model.CombatantEntity
import vcc.dnd4e.model.common.{MinionHealthDefinition, CombatantType}
import vcc.dnd4e.domain.tracker.common._
import vcc.controller.message.TransactionalAction
import collection.mutable.Queue

@RunWith(classOf[JUnitSuiteRunner])
class CombatStateActionHandlerTest extends JUnit4(CombatStateActionHandlerSpec)

object CombatStateActionHandlerSpec extends Specification with Mockito {
  class PartialCombatController(rules: CombatStateRules, state: CombatState, queue: Queue[TransactionalAction])
          extends CombatController(rules, state, queue)
                  with CombatStateActionHandler

  val mOrder = mock[InitiativeOrder]
  val mRoster = mock[CombatantRoster]
  val mMeta = mock[CombatMetaData]
  val mRule = mock[CombatStateRules]
  val mSource = mock[CommandSource]

  val state = new CombatState(mOrder, mRoster, mMeta)
  val aCombatController = new PartialCombatController(mRule, state, new Queue[TransactionalAction])

  "aCombatController handling a StartCombat" should {
    "start combat if not started and has combatant in order" in {
      mMeta.inCombat returns false
      mRule.hasActingCombatant(any[CombatStateView]) returns true
      aCombatController.dispatch(new Transaction(), mSource, StartCombat())
      there was atLeastOne(mMeta).inCombat
      there was one(mMeta).startCombat()(any[Transaction])
      there was one(mOrder).startCombat()(any[Transaction])
      there was one(mRule).hasActingCombatant(any[CombatStateView])
    }

    "throw exception if already started" in {
      mMeta.inCombat returns true
      mRule.hasActingCombatant(any[CombatStateView]) returns true
      aCombatController.dispatch(new Transaction(), mSource, StartCombat()) must throwA(new IllegalActionException("Combat already started."))
      there was atLeastOne(mMeta).inCombat
    }

    "throw exception if no combatant in order" in {
      mMeta.inCombat returns false
      mRule.hasActingCombatant(any[CombatStateView]) returns false
      aCombatController.dispatch(new Transaction(), mSource, StartCombat()) must throwA(new IllegalActionException("Must have at least on combatant in order."))
      there was atLeastOne(mMeta).inCombat
      there was one(mRule).hasActingCombatant(any[CombatStateView])
    }
  }

  "aCombatController handling a EndCombat" should {
    "end combat if started" in {
      mMeta.inCombat returns true
      aCombatController.dispatch(new Transaction(), mSource, EndCombat())
      there was atLeastOne(mMeta).inCombat
      there was one(mMeta).endCombat()(any[Transaction])
      there was one(mOrder).clearOrder()(any[Transaction])
    }

    "throw exception if already started" in {
      mMeta.inCombat returns false
      aCombatController.dispatch(new Transaction(), mSource, EndCombat()) must throwA(new IllegalActionException("Combat already ended."))
      there was atLeastOne(mMeta).inCombat
    }
  }

  "aCombatController handling a AddComabatant" should {
    val entity = CombatantEntity(EntityID.fromName("Dummy"), "Goblin", MinionHealthDefinition(), 4, CombatantType.Monster, null)
    val combA = CombatantID("A")
    val combB = CombatantID("B")

    "add a single combatant" in {
      val trans = new Transaction()
      aCombatController.dispatch(trans, mSource, AddCombatants(List(CombatantRosterDefinition(combA, null, entity))))
      there was one(mRoster).addCombatant(combA, null, entity)(trans)
    }

    "add multiple combatants" in {
      val trans = new Transaction()
      aCombatController.dispatch(trans, mSource, AddCombatants(
        List(CombatantRosterDefinition(combA, null, entity), CombatantRosterDefinition(combB, "007", entity))))
      there was one(mRoster).addCombatant(combA, null, entity)(trans)
      there was one(mRoster).addCombatant(combB, "007", entity)(trans)
    }

    "replace a combatant if already present" in {
      val trans = new Transaction()
      aCombatController.dispatch(trans, mSource, AddCombatants(List(CombatantRosterDefinition(combA, null, entity))))
      there was one(mRoster).addCombatant(combA, null, entity)(trans)
    }
  }

  "aCombatController handling a SetInitiative" should {
    val combA = CombatantID("A")
    val combB = CombatantID("B")
    val iDef1 = InitiativeDefinition(combA, 4, List(10))
    val iDef2 = InitiativeDefinition(combB, 4, List(11))

    "iterate through all in the list" in {
      val trans = new Transaction()
      mRule.canCombatantRollInitiative(state, combA) returns true
      mRule.canCombatantRollInitiative(state, combB) returns true
      aCombatController.dispatch(trans, mSource, SetInitiative(List(iDef1, iDef2)))
      there was one(mRule).canCombatantRollInitiative(state, combA)
      there was one(mRule).canCombatantRollInitiative(state, combB)
      there was one(mOrder).setInitiative(iDef1)(trans)
      there was one(mOrder).setInitiative(iDef2)(trans)
    }

    "throw exception if someone can't roll initiative" in {
      val trans = new Transaction()
      mRule.canCombatantRollInitiative(state, combA) returns true
      mRule.canCombatantRollInitiative(state, combB) returns false
      aCombatController.dispatch(trans, mSource, SetInitiative(List(iDef1, iDef2))) must throwAn(new IllegalActionException("Combatant " + iDef2.combId + " cant roll initiative."))

    }
  }

  "aCombatController handling a SetCombatComment" should {
    "update the comment" in {
      val trans = new Transaction()
      aCombatController.dispatch(trans, mSource, SetCombatComment("and there was light"))
      there was one(mMeta).comment_=("and there was light")(trans)
    }
  }
}