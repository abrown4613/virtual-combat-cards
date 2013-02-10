/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.tracker.common

import org.junit._
import Assert._

import vcc.dnd4e.tracker.common.HealthStatus._

class HealthTrackingTest {

  @Test
  def checkCharacterDefinitionStatus() {
    val hd = CharacterHealthDefinition(21)

    validateHealth(HealthTracker(21, 0, 0, hd), Ok)
    validateHealth(HealthTracker(11, 0, 0, hd), Ok)
    validateHealth(HealthTracker(10, 0, 0, hd), Bloody)
    validateHealth(HealthTracker(10, 0, 1, hd), Bloody)
    validateHealth(HealthTracker(0, 0, 0, hd), Dying)
    validateHealth(HealthTracker(-5, 0, 0, hd), Dying)
    validateHealth(HealthTracker(-5, 0, 1, hd), Dying)
    validateHealth(HealthTracker(-5, 0, 2, hd), Dying)
    validateHealth(HealthTracker(-5, 0, 3, hd), Dead)
    validateHealth(HealthTracker(10, 0, 3, hd), Dead)
  }

  @Test
  def checkMonsterDefinitionStatus() {
    val hd = MonsterHealthDefinition(21)

    validateHealth(HealthTracker(21, 0, 0, hd), Ok)
    validateHealth(HealthTracker(11, 0, 0, hd), Ok)
    validateHealth(HealthTracker(10, 0, 0, hd), Bloody)
    validateHealth(HealthTracker(0, 0, 0, hd), Dying)
    validateHealth(HealthTracker(-5, 0, 0, hd), Dying)
    validateHealth(HealthTracker(-5, 0, 3, hd), Dead)
  }

  @Test
  def checkMinionDefinitionStatus() {
    val hd = MinionHealthDefinition

    validateHealth(HealthTracker(1, 0, 0, hd), Ok)
    validateHealth(HealthTracker(0, 0, 0, hd), Dying)
    validateHealth(HealthTracker(-5, 0, 3, hd), Dead)
  }

  @Test
  def testMinion() {

    var minion: HealthTracker = HealthTracker.createTracker(CombatantType.Minion, 1)

    // Cant get temporary HP
    val modifiedMinion = minion.setTemporaryHitPoints(10)
    assertEquals(minion, modifiedMinion)

    // Any damage should become one, and cause him to be dead
    minion = minion.applyDamage(1)
    assertEquals(0, minion.currentHP)
    assertEquals(Dead, minion.status)

    // This cant raise anyone
    minion = minion.heal(10)
    assertEquals(0, minion.currentHP)
    assertEquals(Dead, minion.status)
  }

  @Test
  def testMonster() {
    var monster: HealthTracker = HealthTracker.createTracker(CombatantType.Monster, 45)
    monster = monster.applyDamage(10)
    assertEquals(35, monster.currentHP)
    assertEquals(Ok, monster.status)

    monster = monster.applyDamage(13)
    assertEquals(22, monster.currentHP)
    assertEquals(Bloody, monster.status)

    monster = monster.heal(11)
    assertEquals(33, monster.currentHP)
    assertEquals(Ok, monster.status)

    monster = monster.heal(20)
    assertEquals(45, monster.currentHP)
    assertEquals(Ok, monster.status)

    monster = monster.setTemporaryHitPoints(10)
    assertEquals(45, monster.currentHP)
    assertEquals(10, monster.temporaryHP)
    assertEquals(Ok, monster.status)
    validateHealth(monster, Ok)

    monster = monster.applyDamage(7)
    assertEquals(monster.base.totalHP, monster.currentHP)
    assertEquals(3, monster.temporaryHP)
    assertEquals(Ok, monster.status)

    monster = monster.setTemporaryHitPoints(5)
    assertEquals(monster.base.totalHP, monster.currentHP)
    assertEquals(5, monster.temporaryHP)
    assertEquals(Ok, monster.status)

    monster = monster.applyDamage(7)
    assertEquals(monster.base.totalHP - 2, monster.currentHP)
    assertEquals(0, monster.temporaryHP)
    assertEquals(Ok, monster.status)

    monster = monster.applyDamage(100)
    assertEquals(0, monster.currentHP)
    assertEquals(Dead, monster.status)

    monster = monster.heal(10)
    assertEquals(0, monster.currentHP)
    assertEquals(Dead, monster.status)

    monster = monster.raiseFromDead()
    assertEquals(0, monster.currentHP)
    assertEquals(Dying, monster.status)
    monster = monster.heal(10)
    assertEquals(10, monster.currentHP)
    assertEquals(Bloody, monster.status)

  }

  @Test
  def testCharacter() {
    var pc: HealthTracker = HealthTracker.createTracker(CombatantType.Character, 45)
    pc = pc.applyDamage(10)
    assertEquals(pc.base.totalHP - 10, pc.currentHP)
    assertEquals(Ok, pc.status)

    pc = pc.applyDamage(13)
    assertEquals((pc.base.totalHP - 10 - 13), pc.currentHP)
    assertEquals(Bloody, pc.status)

    pc = pc.heal(11)
    assertEquals(33, pc.currentHP)
    assertEquals(Ok, pc.status)

    pc = pc.heal(20)
    assertEquals(pc.base.totalHP, pc.currentHP)
    assertEquals(Ok, pc.status)

    pc = pc.setTemporaryHitPoints(10)
    assertEquals(pc.base.totalHP, pc.currentHP)
    assertEquals(10, pc.temporaryHP)
    assertEquals(Ok, pc.status)

    pc = pc.applyDamage(7)
    assertEquals(pc.base.totalHP, pc.currentHP)
    assertEquals(3, pc.temporaryHP)
    assertEquals(Ok, pc.status)

    pc = pc.setTemporaryHitPoints(2)
    assertEquals(3, pc.temporaryHP)
    assertEquals(Ok, pc.status)


    pc = pc.setTemporaryHitPoints(5)
    assertEquals(pc.base.totalHP, pc.currentHP)
    assertEquals(5, pc.temporaryHP)
    assertEquals(Ok, pc.status)

    pc = pc.applyDamage(7)
    assertEquals(pc.base.totalHP - 2, pc.currentHP)
    assertEquals(0, pc.temporaryHP)
    assertEquals(Ok, pc.status)

    pc = pc.applyDamage(50)
    assertEquals(pc.base.totalHP - 52, pc.currentHP)
    assertEquals(Dying, pc.status)

    pc = pc.heal(10)
    assertEquals(10, pc.currentHP)
    assertEquals(Bloody, pc.status)

    pc = pc.applyDamage(35)
    assertEquals(-22, pc.currentHP)
    assertEquals(Dead, pc.status)
  }

  @Test
  def testCharacterDeath() {
    var pc = HealthTracker.createTracker(CombatantType.Character, 40)

    // Bug make sure can't strike living
    pc = pc.failDeathSave()
    assertEquals(0, pc.deathStrikes)


    pc = pc.applyDamage(50)
    assertEquals(40 - 50, pc.currentHP)
    assertEquals(Dying, pc.status)
    assertEquals(0, pc.deathStrikes)

    pc = pc.failDeathSave()
    assertEquals(40 - 50, pc.currentHP)
    assertEquals(Dying, pc.status)
    assertEquals(1, pc.deathStrikes)

    pc = pc.heal(25)
    assertEquals(25, pc.currentHP)
    assertEquals(Ok, pc.status)

    pc = pc.applyDamage(25)
    assertEquals(0, pc.currentHP)
    assertEquals(Dying, pc.status)
    assertEquals(1, pc.deathStrikes)


    pc = pc.failDeathSave()
    assertEquals(0, pc.currentHP)
    assertEquals(Dying, pc.status)
    assertEquals(2, pc.deathStrikes)

    pc = pc.applyDamage(5)
    assertEquals(-5, pc.currentHP)
    assertEquals(Dying, pc.status)
    assertEquals(2, pc.deathStrikes)

    pc = pc.failDeathSave()
    assertEquals(-5, pc.currentHP)
    assertEquals(3, pc.deathStrikes)
    assertEquals(Dead, pc.status)

    // Healing will not raise PC
    pc = pc.heal(10)
    assertEquals(-5, pc.currentHP)
    assertEquals(Dead, pc.status)
    assertEquals(3, pc.deathStrikes)

  }

  @Test
  def testUnboundHealBug() {
    var pc = HealthTracker.createTracker(CombatantType.Character, 20)
    pc = pc.applyDamage(25)
    assertEquals(-5, pc.currentHP)
    pc = pc.heal(300)
    assertEquals(20, pc.currentHP)
  }


  @Test
  def testRaiseDeadLogic() {
    var pc = HealthTracker.createTracker(CombatantType.Character, 20)
    pc = pc.applyDamage(30)
    assertEquals(Dead, pc.status)
    pc = pc.raiseFromDead()
    assertEquals(0, pc.currentHP)
    assertNotEquals(Dead, pc.status)

    var monst = HealthTracker.createTracker(CombatantType.Monster, 20)
    monst = monst.applyDamage(20)
    assertEquals(Dead, monst.status)
    monst = monst.raiseFromDead()
    assertEquals(0, monst.currentHP)
    assertNotEquals(Dead, monst.status)

    monst = HealthTracker.createTracker(CombatantType.Minion, 20)
    monst = monst.applyDamage(20)
    assertEquals(Dead, monst.status)
    monst = monst.raiseFromDead()
    assertEquals(0, monst.currentHP)
    assertNotEquals(Dead, monst.status)
  }

  @Test
  def testRestLogic() {
    var pc = HealthTracker.createTracker(CombatantType.Character, 20)

    //Make him dying
    pc = pc.applyDamage(25)
    pc = pc.failDeathSave()
    assertEquals(Dying, pc.status)
    assertEquals(1, pc.deathStrikes)

    // Fork to a dead by strikes
    var deadPC = pc.failDeathSave()
    deadPC = deadPC.failDeathSave()
    assertEquals(3, deadPC.deathStrikes)
    assertEquals(Dead, deadPC.status)

    // No more strikes but still dying
    val pcAfterShortRest = pc.rest(RestDuration.ShortRest)
    assertEquals(pc.currentHP, pcAfterShortRest.currentHP)
    assertEquals(0, pcAfterShortRest.deathStrikes)
    assertEquals(Dying, pcAfterShortRest.status)

    // Dying PC should go back to ok
    val pcAfterExtendedRest = pc.rest(RestDuration.ExtendedRest)
    assertEquals(pcAfterExtendedRest.base.totalHP, pcAfterExtendedRest.currentHP)
    assertEquals(0, pcAfterExtendedRest.deathStrikes)
    assertEquals(Ok, pcAfterExtendedRest.status)

    // Dead will not improve, and not change at all
    var restedDeadPC = deadPC.rest(RestDuration.ShortRest)
    assertEquals(deadPC, restedDeadPC)
    assertEquals(3, restedDeadPC.deathStrikes)
    assertEquals(Dead, restedDeadPC.status)

    // Dead still dead after long rest
    restedDeadPC = deadPC.rest(RestDuration.ExtendedRest)
    assertEquals(deadPC, restedDeadPC)
    assertEquals(3, restedDeadPC.deathStrikes)
    assertEquals(Dead, restedDeadPC.status)

  }

  @Test
  def testDeltaApply() {
    val pc = HealthTracker.createTracker(CombatantType.Character, 20)

    val pc_mode = HealthTracker(pc.currentHP - 5, 4, 1, pc.base)

    val delta = pc_mode.getDelta

    assertEquals(HealthTrackerDelta(5, 4, 1), delta)

    val pc_mode2 = pc.applyDelta(delta)

    assertEquals(pc_mode, pc_mode2)

    val pc30 = HealthTracker.createTracker(CombatantType.Character, 30)
    val pc30_mode = pc_mode.replaceHealthDefinition(pc30.base)

    assertEquals(HealthTracker(pc30.currentHP - 5, 4, 1, pc30.base), pc30_mode)
  }

  private def validateHealth(health: HealthTracker, expectedStatus: HealthStatus.Value) {
    val formatted = if (health.temporaryHP > 0)
      "%d / %d +%d".format(health.currentHP, health.base.totalHP, health.temporaryHP)
    else
      "%d / %d".format(health.currentHP, health.base.totalHP)

    assertEquals(formatted, health.formattedHitPoints)
    val formattedDeath = health.status.toString + (if(health.status == Dying)  " (%d/3)".format(health.deathStrikes) else  "!!!".substring(0, health.deathStrikes))
    assertEquals(formattedDeath, health.formattedStatus)
    assertEquals(expectedStatus, health.status)
  }
}