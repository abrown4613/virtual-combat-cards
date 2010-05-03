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
package vcc.dnd4e.view.helper

import vcc.dnd4e.view.dialog.InitiativeDialogEntry
import vcc.util.DiceBag
import scala.util.Sorting.stableSort
import vcc.dnd4e.domain.tracker.common.{InitiativeDefinition, CombatantID}

object InitiativeRoller {
  case class GroupInitEntry(roll: Int, initBonus: Int, ids: Set[CombatantID]) extends Ordered[GroupInitEntry] {
    def compare(that: GroupInitEntry): Int = {
      var diff = (this.roll + this.initBonus) - (that.roll + that.initBonus)
      if (diff == 0) {
        diff = this.initBonus - that.initBonus
        if (diff == 0) return if (DiceBag.flipCoin) -1 else 1
      }
      -diff
    }

    def rollIfZeroed(): GroupInitEntry = {
      if (roll <= 0) GroupInitEntry(DiceBag.D(20), initBonus, ids)
      else this
    }
  }

  /**
   * Create Initiative Groups, this will collect essencial data. If initiative
   * values are present the smallest one of the group is used
   * @param joinSimilar Join objects that have the same name
   * @param ie InitiativeDialogEntries that contain the values you need
   * @return a list of groups to be sorted (roll, initiative bonus, Set[Symbol])
   */
  def createInitiativeGroups(joinSimilar: Boolean, ide: List[InitiativeDialogEntry]): List[GroupInitEntry] = {
    val ie = ide.filter(e => !e.reserve)
    if (joinSimilar) {
      var map = scala.collection.mutable.Map.empty[(String, Int), (Int, Set[CombatantID])]
      for (e <- ie) {
        val p = (e.name, e.init)
        if (map.contains(p)) {
          val old = map(p)
          map += p -> (Math.max(e.roll, old._1), old._2 + e.id)
        } else {
          map += p -> (e.roll, Set(e.id))
        }
      }
      map.map {case ((n, init), (roll, ids)) => GroupInitEntry(roll, init, ids)}.toList
    } else {
      ie.map(e => GroupInitEntry(e.roll, e.init, Set(e.id)))
    }
  }

  /**
   * Sort initiative for each grou and return an ordered sequence of symbols
   * @param groups Groups as created by createInitiativeGroups
   * @return Sequence of symbols
   */
  def sortGroups(groups: List[GroupInitEntry]): List[CombatantID] = {
    val ord = stableSort(groups)
    val ordIds = ord.map(x => stableSort(x.ids.toSeq, (a: CombatantID, b: CombatantID) => {a.id <= b.id}))
    ordIds.flatMap(x => x.toList).toList
  }

  /**
   * Roll initiative for the group. 
   * @param joinSimilar Join objects that have the same name
   * @param ie InitiativeDialogEntries that contain the values you need
   * @return Sequence of symbols ordered by initiative
   */
  def rollInitiative(joinSimilar: Boolean, ie: List[InitiativeDialogEntry]): List[InitiativeDefinition] = {
    //FIXME Initiative rolling has to be remade
    var l = createInitiativeGroups(joinSimilar, ie)
    l = l.map(e => e.rollIfZeroed)
    val finalList = sortGroups(l).toList
    (finalList.length to 1 by -1).toList.zip(finalList).map(p => InitiativeDefinition(p._2, 0, List(p._1)))
  }
}
