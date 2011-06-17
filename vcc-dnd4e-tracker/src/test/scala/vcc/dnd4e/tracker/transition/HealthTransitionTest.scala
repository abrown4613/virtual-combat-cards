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
package vcc.dnd4e.tracker.transition

import org.specs2.SpecificationWithJUnit
import org.specs2.mock.Mockito
import vcc.dnd4e.tracker.StateLensFactory
import vcc.dnd4e.tracker.common.{CombatantID, HealthTracker, CombatState}
import vcc.scalaz.Lens
import org.specs2.matcher.{Expectable, Matcher}

class HealthTransitionTest extends SpecificationWithJUnit {
  def is = {
    "DamageTransition" ^ test(Damage()) ^
      "HealTransition" ^ test(Heal()) ^
      "SetTemporaryHPTransition" ^ test(SetTemp()) ^
      "FailDeathSaveTransition" ^ test(DeathSave()) ^
      "RevertDeathTransition" ^ test(Undead()) ^
      end
  }

  private def test(hs: HealthSetup) =
    "fail on non existante combatant" ! hs.caseCombatantNotFound() ^
      "update health tracker in correct way" ! hs.updateHealth() ^
      endp

  trait HealthSetup extends Mockito {
    protected val combFound = CombatantID("A")
    protected val combNotFound = CombatantID("NOT")
    protected val mState = mock[CombatState]
    protected val mNewState = mock[CombatState]
    protected val mHealth = mock[HealthTracker]
    protected val mHealth2 = mock[HealthTracker]
    protected val mLF = mock[StateLensFactory]
    protected val mHL = mock[Lens[CombatState, HealthTracker]]

    mLF.combatantHealth(combFound) returns mHL
    mLF.combatantHealth(combNotFound) throws new NoSuchElementException("Combatant not Found")
    mHL.modIfChanged(be_==(mState), FuncCall(mHealth, mHealth2)) returns mNewState

    //Overide this method to create behavior under test
    def createTransition(comb: CombatantID): HealthTransition

    def caseCombatantNotFound() = createTransition(combNotFound).transition(mLF, mState) must throwA[NoSuchElementException]

    def updateHealth() = {
      (createTransition(combFound).transition(mLF, mState) must_== mNewState) and
        (there was one(mHL).modIfChanged(beEqualTo(mState), FuncCall(mHealth, mHealth2)))
    }
  }

  case class Damage() extends HealthSetup {
    def createTransition(comb: CombatantID): HealthTransition = {
      mHealth.applyDamage(10) returns mHealth2
      DamageTransition(comb, 10)
    }
  }

  case class Heal() extends HealthSetup {
    def createTransition(comb: CombatantID): HealthTransition = {
      mHealth.heal(12) returns mHealth2
      HealTransition(comb, 12)
    }
  }

  case class SetTemp() extends HealthSetup {
    def createTransition(comb: CombatantID): HealthTransition = {
      mHealth.setTemporaryHitPoints(15, false) returns mHealth2
      SetTemporaryHPTransition(comb, 15)
    }
  }

  case class DeathSave() extends HealthSetup {
    def createTransition(comb: CombatantID): HealthTransition = {
      mHealth.failDeathSave() returns mHealth2
      FailDeathSaveTransition(comb)
    }
  }

  case class Undead() extends HealthSetup {
    def createTransition(comb: CombatantID): HealthTransition = {
      mHealth.raiseFromDead() returns mHealth2
      RevertDeathTransition(comb)
    }
  }


  case class FuncCall[A, B](in: A, out: B) extends Matcher[Function1[A, B]] {
    def apply[S <: Function1[A, B]](t: Expectable[S]) = {
      val calculated = t.value(in)
      result(calculated == out, t.description + " evaluates to " + calculated, t.description + " does not evaluates to " + calculated, t)
    }
  }

}