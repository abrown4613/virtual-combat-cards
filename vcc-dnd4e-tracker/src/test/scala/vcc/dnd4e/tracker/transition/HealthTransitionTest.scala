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
import org.specs2.mock.Mockito
import vcc.dnd4e.tracker.StateLensFactory
import vcc.dnd4e.tracker.common._

class HealthTransitionTest extends SpecificationWithJUnit with SampleStateData {
  def is = {
    "DamageTransition" ^ test(Damage()) ^
      "DamageTransition should shutdown combat" ! Damage().testLastLivingDeath ^
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
    protected val mHealth = mock[HealthTracker]
    protected val mHealth2 = mock[HealthTracker]

    protected val stateBuilder = StateBuilder.emptyState().
      addCombatant(Some(combA), null, entityPc1).
      addCombatant(None, null, entityMonster).
      addCombatant(None, null, entityMonster).
      setInitiative(combA, 10).
      setInitiative(comb1, 12)


    //Override this method to create behavior under test
    def createTransition(comb: CombatantID): HealthTransition

    def caseCombatantNotFound() = createTransition(combB).transition(stateBuilder.done) must throwA[NoSuchElementException]

    def updateHealth() = {
      val state = stateBuilder.modifyHealth(combA, x => mHealth).done
      val nState = createTransition(combA).transition(state)
      val hl = StateLensFactory.combatantHealth(combA)
      (hl.get(nState) must_== mHealth2) and (state.roster.combatantDiff(nState.roster) must_== Set(CombatantDiff(combA, mHealth, mHealth2)))
    }
  }

  case class Damage() extends HealthSetup {
    def createTransition(comb: CombatantID): HealthTransition = {
      mHealth.applyDamage(10) returns mHealth2
      mHealth2.status returns HealthStatus.Ok
      DamageTransition(comb, 10)
    }

    def testLastLivingDeath = {
      val state = stateBuilder.modifyHealth(comb1, ht => ht.applyDamage(1000)).done
      val trans = DamageTransition(combA, 1000)
      val nState = trans.transition(state.startCombat())

      (nState.isCombatStarted must beFalse)
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

  def functionCall[A, B](in: A, out: B) = be_==(out) ^^ {
    (f: Function1[A, B]) => f(in)
  }

}