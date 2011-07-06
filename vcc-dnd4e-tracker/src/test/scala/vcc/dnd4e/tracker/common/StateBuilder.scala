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
package vcc.dnd4e.tracker.common

import vcc.dnd4e.tracker.StateLensFactory

/**
 * Use this trait to add a common set of combatant information
 */
trait SampleStateData {
  val combA = CombatantID("A")
  val ioA0 = InitiativeOrderID(combA, 0)
  val ioA1 = InitiativeOrderID(combA, 1)

  val combB = CombatantID("B")
  val ioB0 = InitiativeOrderID(combB, 0)

  val comb1 = CombatantID("1")
  val io1_0 = InitiativeOrderID(comb1, 0)

  val comb2 = CombatantID("2")
  val io2_0 = InitiativeOrderID(comb2, 0)

  //Val CombatantEntity
  val entityPc1 = CombatantEntity(null, "Fighter", CharacterHealthDefinition(40), 2, CombatantType.Character, null)
  val entityPc2 = CombatantEntity(null, "Mage", CharacterHealthDefinition(25), 4, CombatantType.Character, null)
  val entityMinion = CombatantEntity(null, "Minion", MinionHealthDefinition, 1, CombatantType.Minion, null)
  val entityMonster = CombatantEntity(null, "Monster", MonsterHealthDefinition(30), 1, CombatantType.Minion, null)

}

class StateBuilder(private var state: CombatState) {
  val lf = StateLensFactory

  /**
   * Add combatant to the combat
   */
  def addCombatant(cid: Option[CombatantID], alias: String, entity: CombatantEntity): StateBuilder = {
    state = lf.rosterLens.mod(state, _.addCombatant(cid, alias, entity))
    this
  }

  /**
   * Modify effect list
   */
  def modifyEffectList(cid: CombatantID, change: EffectList => EffectList): StateBuilder = {
    state = lf.combatantEffectList(cid).mod(state, change)
    this
  }

  /**
   * Modify health tracker
   */
  def modifyHealth(cid: CombatantID, change: HealthTracker => HealthTracker): StateBuilder = {
    state = lf.combatantHealth(cid).mod(state, change)
    this
  }

  /**
   * Modify initiative tracker
   */
  def modifyInitiative(ioi: InitiativeOrderID, change: InitiativeTracker => InitiativeTracker): StateBuilder = {
    state = lf.initiativeTrackerLens(ioi).mod(state, change)
    this
  }

  /**
   * Sets initiative, bonus will be picked from definition.
   * @param cid ID of combatant to set
   * @param rolls Set of rolls to initiative
   */
  def setInitiative(cid: CombatantID, rolls: Int*): StateBuilder = {
    val init = lf.combatant(cid).get(state).definition.entity.initiative
    state = lf.orderLens.mod(state, o => o.setInitiative(InitiativeDefinition(cid, init, rolls.toList)))
    this
  }

  /**
   * StartCombat
   */
  def startCombat(): StateBuilder = {
    state = state.startCombat()
    this
  }

  def done = state
}

object StateBuilder {
  def emptyState() = new StateBuilder(CombatState.empty)

  /**
   * Simple transformation to kill a combatant
   */
  def kill: HealthTracker => HealthTracker = ht => ht.applyDamage(ht.base.totalHP * 2)
}