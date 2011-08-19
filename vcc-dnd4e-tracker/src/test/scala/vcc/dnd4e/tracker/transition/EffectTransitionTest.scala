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
package vcc.dnd4e.tracker.transition

import org.specs2.SpecificationWithJUnit
import vcc.dnd4e.tracker.common.Effect.Condition
import vcc.dnd4e.tracker.StateLensFactory
import org.specs2.mock.Mockito
import vcc.dnd4e.tracker.common._
import vcc.controller.IllegalActionException

class EffectTransitionTest extends SpecificationWithJUnit with SampleStateData {
  val eid = EffectID(combA, 11)

  def is =
    "AddEffectTransition" ^
      "when we find source and target" ! m().testAddEffect ^
      "throw exeception if source no in combat" ! m().testAddEffectWithMissingSource.pendingUntilFixed("TODO Not really need") ^
      endp ^
      "CancelEffectTransition" ! m().cancelEffect ^
      "SustainEffectTransition" ! m().sustainEffect ^
      "UpdateEffectConditionTransition" ! m().testUpdateEffect ^
      endp

  case class m() extends Mockito {

    val mEL = mock[EffectList]
    val mEL2 = mock[EffectList]
    val state: CombatState = StateBuilder.emptyState().
      addCombatant(Some(combA), null, entityPc1).
      addCombatant(Some(combB), null, entityPc2).
      modifyEffectList(combA, x => mEL).
      done

    def testAddEffect = {
      val trans = AddEffectTransition(combA, combB, Condition.Generic("nice", true), Duration.RoundBound(ioA0, Duration.Limit.EndOfTurnSustain))
      mEL.addEffect(combB, Condition.Generic("nice", true), Duration.RoundBound(ioA0, Duration.Limit.EndOfTurnSustain)) returns mEL2
      val nState = trans.transition(state)
      (there was one(mEL).addEffect(combB, Condition.Generic("nice", true), Duration.RoundBound(ioA0, Duration.Limit.EndOfTurnSustain))) and
        (StateLensFactory.combatantEffectList(combA).get(nState) must_== mEL2)
    }

    def testAddEffectWithMissingSource = {
      val trans = AddEffectTransition(combA, comb1, Condition.Generic("nice", true), Duration.RoundBound(ioA0, Duration.Limit.EndOfTurnSustain))

      (trans.transition(state) must throwA(new IllegalActionException(comb1 + " is not in combat"))) and
        (there was no(mEL).addEffect(any, any, any))
    }

    def cancelEffect = {
      val trans = CancelEffectTransition(eid)
      mEL.transformAndFilter(EffectTransformation.cancelEffect(eid)) returns mEL2
      val nState = trans.transition(state)
      (there was one(mEL).transformAndFilter(EffectTransformation.cancelEffect(eid))) and
        (StateLensFactory.combatantEffectList(combA).get(nState) must_== mEL2)
    }

    def sustainEffect = {
      val trans = SustainEffectTransition(eid)
      mEL.transformAndFilter(EffectTransformation.sustainEffect(eid)) returns mEL2
      val nState = trans.transition(state)
      (there was one(mEL).transformAndFilter(EffectTransformation.sustainEffect(eid))) and
        (StateLensFactory.combatantEffectList(combA).get(nState) must_== mEL2)
    }

    def testUpdateEffect = {
      val trans = UpdateEffectConditionTransition(eid, Condition.Generic("other", false))
      mEL.transformAndFilter(EffectTransformation.updateCondition(eid, Condition.Generic("other", false))) returns mEL2
      val nState = trans.transition(state)
      (there was one(mEL).transformAndFilter(EffectTransformation.updateCondition(eid, Condition.Generic("other", false)))) and
        (StateLensFactory.combatantEffectList(combA).get(nState) must_== mEL2)
    }
  }
}