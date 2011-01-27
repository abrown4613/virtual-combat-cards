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
package vcc.dnd4e.view

import org.specs.SpecificationWithJUnit
import org.specs.mock.Mockito
import actors.Actor
import vcc.controller.{Decision, Ruling, TrackerChangeObserver}
import vcc.dnd4e.domain.tracker.snapshot.{StateChange, CombatState, CombatStateWithChanges}
import vcc.infra.prompter.{RulingBroker}

/**
 * Test to make sure RulingBroker gets called when PanelDirector is provoked by the pd.provideDecisionsForRulings.
 */
class PanelDirectorDialogFlowTest extends SpecificationWithJUnit with Mockito {

  private val ruling1 = mock[Ruling]
  private val ruling2 = mock[Ruling]
  private val decision1 = mock[Decision[_ <: Ruling]]
  private val decision2 = mock[Decision[_ <: Ruling]]
  val rulings = List(ruling1, ruling2)
  val mDecisions = List(decision1, decision2)

  "PanelDirector" should {
    val mockRuleBroker = mock[RulingBroker]
    val csm = mock[TrackerChangeObserver[CombatStateWithChanges]]
    csm.getSnapshot() returns CombatStateWithChanges(CombatState(false, "", Nil, Map(), None, Map()), new StateChange())
    val pd = new PanelDirector(mock[Actor], csm, mock[StatusBar], mockRuleBroker)

    "forward request to RulingBroker" in {
      pd.provideDecisionsForRulings(rulings)
      there was one(mockRuleBroker).promptRuling(rulings)
    }
    "send reply back to requestor" in {
      mockRuleBroker.promptRuling(rulings) returns mDecisions
      pd.provideDecisionsForRulings(rulings) must_== mDecisions
    }

    "not forward empty list" in {
      pd.provideDecisionsForRulings(Nil) must_== Nil
      there was no(mockRuleBroker).promptRuling(Nil)
    }
  }

}