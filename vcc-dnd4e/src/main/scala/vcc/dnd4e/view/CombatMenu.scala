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
package vcc.dnd4e.view

import dialog.InitiativeDialog
import scala.swing._
import vcc.dnd4e.domain.tracker.common.Command._
import vcc.dnd4e.domain.tracker.snapshot.StateChange

class CombatMenu(director: PanelDirector, parent: Frame) extends Menu("Combat") with CombatStateObserver {
  private val menuStartCombat = new MenuItem(Action("Start Combat") {
    director requestAction StartCombat()
  })

  private val menuRollInitiative = new MenuItem(Action("Roll Initiative...") {
    val dlg = new InitiativeDialog(parent, director)
    dlg.visible = true
    val res = dlg.dialogResult
    if (res.isDefined) {
      director requestAction SetInitiative(res.get)
    }
  })

  private val menuEndCombat = new MenuItem(Action("End Combat") {
    director requestAction EndCombat()
  })
  private val menuShortRest = new MenuItem(Action("Short Rest") {
    director requestAction ApplyRest(false)
  })
  private val menuExtendedRest = new MenuItem(Action("Extended Rest") {
    director requestAction ApplyRest(true)
  })

  private val menuClearNPC = new MenuItem(Action("Clear Monsters") {
    director requestAction ClearRoster(false)
  })
  private val menuClearAll = new MenuItem(Action("Clear All") {
    director requestAction ClearRoster(true)
  })

  //Initialization
  private val allItems = Seq(menuStartCombat, menuEndCombat, menuShortRest, menuExtendedRest, menuClearNPC, menuClearAll)
  contents ++= Seq(menuRollInitiative, menuStartCombat, menuEndCombat, new Separator, menuShortRest, menuExtendedRest, new Separator, menuClearNPC, menuClearAll)
  allItems.foreach(mi => mi.enabled = false)
  director.registerStateObserver(this)

  def combatStateChanged(newState: UnifiedSequenceTable, changes: StateChange) {
    if (StateChange.hasSequenceChange(changes.changes) || changes.changes.contains(StateChange.combat.MetaData)) {
      menuStartCombat.enabled = !newState.state.isCombatStarted && director.rules.hasActingCombatant(newState.state)
      menuEndCombat.enabled = newState.state.isCombatStarted
      menuShortRest.enabled = !menuEndCombat.enabled
      menuExtendedRest.enabled = !menuEndCombat.enabled
      menuClearAll.enabled = !menuEndCombat.enabled
      menuClearNPC.enabled = !menuEndCombat.enabled
    }
  }
}