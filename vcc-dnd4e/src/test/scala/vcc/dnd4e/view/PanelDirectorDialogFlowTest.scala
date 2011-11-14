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
package vcc.dnd4e.view

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.mock.Mockito
import org.specs2.specification.Scope
import actors.Actor
import vcc.controller.{Decision, Ruling, TrackerChangeObserver}
import vcc.infra.prompter.{RulingBroker}
import vcc.controller.message.TransactionalAction
import vcc.dnd4e.tracker.common.Command.ExecuteInitiativeAction
import vcc.dnd4e.tracker.common._
import vcc.dnd4e.domain.tracker.common._

/**
 * Test to make sure RulingBroker gets called when PanelDirector is provoked by the pd.provideDecisionsForRulings.
 */
class PanelDirectorDialogFlowTest extends SpecificationWithJUnit with Mockito {

  trait context extends Scope {
    val ruling1 = mock[Ruling]
    val ruling2 = mock[Ruling]
    val decision1 = mock[Decision[_ <: Ruling]]
    val decision2 = mock[Decision[_ <: Ruling]]
    val rulings = List(ruling1, ruling2)
    val mDecisions = List(decision1, decision2)
    val mAction = mock[TransactionalAction]
    mAction.description returns "NO-MESSAGE-MIGRATING"

    // TO test real actions
    val combA = CombatantID("A")
    val ioiA = InitiativeOrderID(combA, 0)
    val mState = mock[CombatStateView]
    val mCombA = mock[CombatantStateView]
    mState.combatantViewFromID(combA) returns mCombA
    val combADef = combatantDefinition(combA, "Goblin", "Shorty", CombatantType.Monster)
    mCombA.definition returns combADef
    val mockRuleBroker = mock[RulingBroker]
    val csm = mock[TrackerChangeObserver[CombatStateView]]
    csm.getSnapshot() returns mock[CombatStateView]
    val pd = new PanelDirector(mock[Actor], csm, mock[StatusBar], mockRuleBroker)
  }

  "PanelDirector" should {

    "forward request to RulingBroker" in new context {
      pd.provideDecisionsForRulings(mAction, rulings)
      there was one(mockRuleBroker).promptRuling("NO-MESSAGE-MIGRATING", rulings)
    }
    "send reply back to requestor" in new context {
      mockRuleBroker.promptRuling("NO-MESSAGE-MIGRATING", rulings) returns mDecisions
      pd.provideDecisionsForRulings(mAction, rulings) must_== mDecisions
    }

    "not forward empty list" in new context {
      pd.provideDecisionsForRulings(mAction, Nil) must_== Nil
      there was no(mockRuleBroker).promptRuling(any, any)
    }

    "provide correct message for the end round" in new context {
      csm.getSnapshot() returns mock[CombatStateView]
      val action = ExecuteInitiativeAction(ioiA, InitiativeAction.EndRound)
      val msg = "NO-MESSAGE-MIGRATING"
      mockRuleBroker.promptRuling(msg, rulings) returns mDecisions
      pd.provideDecisionsForRulings(action, rulings)
      there was one(mockRuleBroker).promptRuling(msg, rulings)
    }
  }

  def combatantDefinition(comb: CombatantID, name: String, alias: String, ctype: CombatantType.Value) =
    CombatantRosterDefinition(comb, alias, CombatantEntity(null, name, null, 0, ctype, null))

}
