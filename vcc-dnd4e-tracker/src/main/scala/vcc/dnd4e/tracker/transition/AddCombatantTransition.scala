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

import vcc.controller.IllegalActionException
import vcc.dnd4e.tracker.common._

/**
 * Add combatant to the combat.
 * @param cid Optional ID for the combatant, None is not relevant
 * @param alias Option alias for combatant (null if not present)
 * @param entity CombatantEntity definition
 */
case class AddCombatantTransition(cid: Option[CombatantID], alias: String, entity: CombatantEntity) extends CombatTransition {
  def transition(iState: CombatState): CombatState = {
    val rl = iState.lensFactory.rosterLens
    rl.set(iState, rl.get(iState).addCombatant(cid, alias, entity))
  }
}

/**
 * Set initiative to a single combatant. The combatant must be in the roster, and if combat is started it should not
 * have and initiative.
 * @param iDef Initiative definition for the combatant
 */
case class SetInitiativeTransition(iDef: InitiativeDefinition) extends CombatTransition {
  def transition(iState: CombatState): CombatState = {
    val ol = iState.lensFactory.orderLens
    if (!iState.roster.isDefinedAt(iDef.combId))
      throw new IllegalActionException("Combatant " + iDef.combId + " not in combat roster")
    if (!iState.rules.canCombatantRollInitiative(iState, iDef.combId))
      throw new IllegalActionException("Combatant " + iDef.combId + " is already in order")

    if (ol.get(iState).isInSequence(iDef.combId)) {
      ol.mod(ol.mod(iState, _.removeCombatant(iDef.combId)), _.setInitiative(iDef))
    } else {
      ol.mod(iState, _.setInitiative(iDef))
    }
  }
}

/**
 * Start combat, must happen only when at least one combatant has an inititiave definition, and can not happen twice.
 */
case object StartCombatTransition extends CombatTransition {

  def transition(iState: CombatState): CombatState = {
    if (iState.isCombatStarted)
      throw new IllegalActionException("Combat already started")

    if (!iState.rules.hasActingCombatant(iState))
      throw new IllegalActionException("Must have at least on combatant in order")

    iState.lensFactory.orderLens.mod(iState, _.startCombat())
  }
}

/**
 * Apply rest to all combats.
 * @param isExtended True for Extended Rests, false if we should apply Short Rest
 */
case class RestTransition(isExtended: Boolean) extends CombatTransition {
  def transition(iState: CombatState): CombatState = {
    if (iState.isCombatStarted)
      throw new IllegalActionException("Can not rest during combat")
    // We are ok, apply rest to all entries in roster
    val ids = iState.roster.entries.keys
    val lf = iState.lensFactory
    ids.foldLeft(iState)((s, id) => lf.combatant(id).mod(s, c => c.applyRest(isExtended)))
  }
}

/**
 * Set combat level comment.
 * @param comment Comment to be set
 */
case class SetCombatCommentTransition(comment: String) extends CombatTransition {
  def transition(iState: CombatState): CombatState = {
    iState.copy(comment = Option(comment))
  }
}

/**
 * Clear roster of combatants.
 * @param onlyMonsters Clear only the monsters, if false will clear all.
 */
case class ClearRosterTransition(onlyMonsters: Boolean) extends CombatTransition {
  def transition(iState: CombatState): CombatState = {
    if (iState.isCombatStarted)
      throw new IllegalActionException("Can not clear while in combat")
    if (onlyMonsters) {
      iState.lensFactory.rosterLens.modIfChanged(iState, roster => roster.clear(c => c.definition.entity.ctype != CombatantType.Character))
    } else {
      CombatState.empty
    }
  }
}

/**
 *  End combat.
 */
case object EndCombatTransition extends CombatTransition {
  def transition(iState: CombatState): CombatState = {
    if (!iState.isCombatStarted)
      throw new IllegalActionException("Combat not started")
    iState.endCombat()
  }
}

/**
 * Set combatant specific combatant.
 * @param cid Combatant to set comment
 * @param comment New comment message
 */
case class SetCombatantCommentTransition(cid: CombatantID, comment: String) extends CombatTransition {
  def transition(iState: CombatState): CombatState = {
    iState.lensFactory.combatantComment(cid).set(iState, comment)
  }
}