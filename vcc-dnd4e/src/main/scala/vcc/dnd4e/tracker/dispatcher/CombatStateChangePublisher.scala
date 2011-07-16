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
package vcc.dnd4e.tracker.dispatcher

import vcc.dnd4e.tracker.common._
import vcc.dnd4e.domain.tracker.common._
import vcc.controller.transaction.ChangeNotification

object CombatStateChangePublisher {

  def toAspectSet(comb: Combatant): Pair[CombatantID, Set[CombatantAspect]] = {
    (comb.definition.cid, Set[CombatantAspect](comb.definition, CombatantComment(comb.comment), comb.effects, comb.health))
  }

  def combatantChanges(old: Combatant, current: Combatant): List[ChangeNotification] = {
    var l = List.empty[ChangeNotification]
    val cid = old.definition.cid
    if (old.health != current.health) l = CombatantChange(cid, current.health) :: l
    if (old.effects != current.effects) l = CombatantChange(cid, current.effects) :: l
    if (old.comment != current.comment) l = CombatantChange(cid, CombatantComment(current.comment)) :: l
    if (old.definition != current.definition) l = CombatantChange(cid, current.definition) :: l
    l
  }

  def publish(old: CombatState, current: CombatState): List[ChangeNotification] = {
    var changes = List.empty[ChangeNotification]

    if (current.roster.entries.keySet != old.roster.entries.keySet) {
      changes = RosterChange(Map(current.roster.entries.map(me => toAspectSet(me._2)).toSeq: _*)) :: changes
    } else {
      val o = old.roster.entries
      val diff = current.roster.entries.collect {
        case (k, v) if (o.isDefinedAt(k) && v != o(k)) => (o(k), v)
      }
      changes = diff.flatMap(x => combatantChanges(x._1, x._2)).toList ::: changes
    }
    //Initiative Changes
    if ((old.isCombatStarted != current.isCombatStarted) || (old.comment != current.comment)) {
      changes = CombatMetaDataChange(current.isCombatStarted, current.comment.getOrElse("")) :: changes
    }

    val initChanges: List[ChangeNotification] = {
      val oldOrder = old.order
      val curOrder = current.order
      (if (oldOrder.nextUp != curOrder.nextUp)
        List(InitiativeOrderFirstChange(if (curOrder.nextUp.isDefined) curOrder.nextUp.get else null))
      else Nil) :::
        (if (oldOrder.sequence != curOrder.sequence) List(InitiativeOrderChange(curOrder.sequence.map(curOrder.tracker(_)))) else Nil) :::
        (if (oldOrder.tracker != curOrder.tracker) {
          curOrder.tracker.collect {
            case (k, it) if (oldOrder.tracker.isDefinedAt(k) && it != oldOrder.tracker(k)) => InitiativeTrackerChange(it)
          }.toList
        } else Nil) :::
        Nil
    }
    initChanges ::: changes.reverse
  }
}