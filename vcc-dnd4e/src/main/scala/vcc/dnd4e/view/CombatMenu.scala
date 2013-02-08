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
package vcc.dnd4e.view

import dialog.InitiativeDialog
import scala.swing._
import vcc.dnd4e.tracker.common.Command._
import vcc.dnd4e.tracker.common.UnifiedSequenceTable

class CombatMenu(director: PanelDirector, parent: Frame) extends Menu("Combat") with CombatStateObserver {
  private val menuStartCombat = createActionRequestMenuItem("Start Combat", StartCombat())
  private val menuRollInitiative = createInitiativeDialogMenuItem()
  private val menuEndCombat = createActionRequestMenuItem("End Combat", EndCombat())
  private val menuShortRest = createActionRequestMenuItem("Short Rest", ApplyRest(false))
  private val menuExtendedRest = createActionRequestMenuItem("Extended Rest", ApplyRest(true))
  private val menuClearNPC = createActionRequestMenuItem("Clear Monsters", ClearRoster(false))
  private val menuClearAll = createActionRequestMenuItem("Clear All", ClearRoster(true))
  private var combatState: UnifiedSequenceTable = null

  contents ++= Seq(menuRollInitiative, menuStartCombat, menuEndCombat, new Separator, menuShortRest, menuExtendedRest, new Separator, menuClearNPC, menuClearAll)
  director.registerStateObserver(this)

  def combatStateChanged(newState: UnifiedSequenceTable) {
    combatState = newState
    menuStartCombat.enabled = !newState.state.isCombatStarted && director.rules.hasActingCombatant(newState.state)
    menuEndCombat.enabled = newState.state.isCombatStarted
    menuShortRest.enabled = !menuEndCombat.enabled
    menuExtendedRest.enabled = !menuEndCombat.enabled
    menuClearAll.enabled = !menuEndCombat.enabled
    menuClearNPC.enabled = !menuEndCombat.enabled
    menuRollInitiative.enabled = !newState.state.allCombatantIDs.isEmpty
  }

  private def createActionRequestMenuItem(label: String, action: CombatStateAction): MenuItem = {
    val menuItem = new MenuItem(Action(label) {
      director requestAction action
    })
    menuItem.enabled = false
    menuItem
  }

  private def createInitiativeDialogMenuItem(): MenuItem = {
    val menuItem = new MenuItem(Action("Roll Initiative...") {
      val dlg = new InitiativeDialog(parent, director, combatState)
      //Result is Pair (Boolean, List[InitiativeDefinition])
      val res = dlg.promptUser()
      if (res.isDefined) {
        val (startCombat, initiative) = res.get
        director requestAction SetInitiative(initiative)
        if (startCombat && !initiative.isEmpty) director requestAction StartCombat()
      }
    })
    menuItem.enabled = false
    menuItem
  }
}