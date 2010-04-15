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
import vcc.dnd4e.domain.tracker.common.Action._
import vcc.dnd4e.domain.tracker.common.{CombatStateView, CombatStateRules}

@RunWith(classOf[JUnitSuiteRunner])
class CombatStateActionHandlerTest extends JUnit4(CombatStateActionHandlerSpec)

object CombatStateActionHandlerSpec extends Specification with Mockito {
  val mOrder = mock[InitiativeOrder]
  val mRoster = mock[CombatantRoster]
  val mMeta = mock[CombatMetaData]
  val mRule = mock[CombatStateRules]
  val mSource = mock[CommandSource]

  val aCombatController = new CombatController(mRule, new CombatState(mOrder, mRoster, mMeta))

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

}