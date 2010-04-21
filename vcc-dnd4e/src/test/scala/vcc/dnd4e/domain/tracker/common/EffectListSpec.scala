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
package vcc.dnd4e.domain.tracker.common


import org.specs.Specification
import org.junit.runner.RunWith
import org.specs.runner.{JUnit4, JUnitSuiteRunner}
import org.specs.mock.Mockito

@RunWith(classOf[JUnitSuiteRunner])
class EffectListTest extends JUnit4(EffectListSpec)

object EffectListSpec extends Specification with Mockito {
  import Effect._

  val combA = CombatantID("A")
  val combB = CombatantID("B")
  val anyCondition = Effect.Condition.Generic("any")

  val aBigList = EffectList(combA, List(
    Effect(EffectID(combA, 0), combA, anyCondition, true, Duration.Stance),
    Effect(EffectID(combA, 1), combA, anyCondition, true, Duration.Rage)))

  "an EffectList" should {

    "add an effect providing an effect ID" in {
      val blankList = EffectList(combB, Nil)

      val nList = blankList.addEffect(combA, anyCondition, false, Duration.SaveEnd)

      nList.effects mustNot beEmpty
      nList.effects(0).effectId must notBeNull
      nList.effects(0).effectId.combId must_== combB
    }

    "when adding to a list the new effect must have a unique ID" in {
      val nList = aBigList.addEffect(combB, anyCondition, false, Duration.SaveEnd)
      (nList.effects -- aBigList.effects).length must_== 1
      val newEntry = getFirstNewEffect(aBigList, nList)
      aBigList.effects mustNot exist(e => e.effectId == newEntry.effectId)
    }

    "replace a temporary mark by a new mark" in {
      val nList = aBigList.addEffect(combB, Condition.Mark(combB, false), false, Duration.Other)
      val mark1 = getFirstNewEffect(aBigList, nList)
      val nnList = nList.addEffect(combB, Condition.Mark(combB, false), false, Duration.SaveEnd)
      val mark2 = getFirstNewEffect(nList, nnList)

      mark1 must notBeNull
      mark2 must notBeNull
      mark1.effectId must_!= mark2.effectId
      mark2.duration must_== Duration.SaveEnd
    }

    "keep the permanent mark when adding a new mark" in {
      val nList = aBigList.addEffect(combB, Condition.Mark(combB, true), false, Duration.Other)
      val mark1 = getFirstNewEffect(aBigList, nList)
      val nnList = nList.addEffect(combB, Condition.Mark(combB, false), false, Duration.SaveEnd)
      val mark2 = getFirstNewEffect(nList, nnList)

      mark1 must notBeNull
      mark2 must beNull
    }

    "replace a Stance duration if found" in {
      val nList = aBigList.addEffect(combA, anyCondition, true, Duration.Stance)
      val newEntry = getFirstNewEffect(aBigList, nList)
      nList.effects must notExist(e => e.duration == Duration.Stance && e.effectId != newEntry.effectId)
    }

    "replace a Rage duration if in list" in {
      val nList = aBigList.addEffect(combA, anyCondition, true, Duration.Rage)
      val newEntry = getFirstNewEffect(aBigList, nList)
      nList.effects must notExist(e => e.duration == Duration.Rage && e.effectId != newEntry.effectId)
    }

    "apply transformation to all elements" in {
      val mTransformation = mock[EffectTransformation]

      mTransformation.transform(aBigList.effects(0)) returns aBigList.effects(0)
      mTransformation.transform(aBigList.effects(1)) returns aBigList.effects(1)

      aBigList.transformAndFilter(mTransformation)

      there was one(mTransformation).transform(aBigList.effects(0)) then
              one(mTransformation).transform(aBigList.effects(1))
    }

    "filter out any transformation return as null" in {
      val mTransformation = mock[EffectTransformation]

      mTransformation.transform(aBigList.effects(0)) returns aBigList.effects(0)
      mTransformation.transform(aBigList.effects(1)) returns null

      val nList = aBigList.transformAndFilter(mTransformation)

      (aBigList.effects -- nList.effects) must_== List(aBigList.effects(1))

      there was one(mTransformation).transform(aBigList.effects(0)) then
              one(mTransformation).transform(aBigList.effects(1))
    }

  }

  def getFirstNewEffect(oList: EffectList, nList: EffectList): Effect = {
    val diff = nList.effects -- oList.effects
    if (diff.isEmpty) null
    else diff(0)
  }
}