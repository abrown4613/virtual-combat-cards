/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
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
package vcc.dnd4e.view.dialog


import scala.swing._
import scala.swing.event._
import vcc.util.swing._

import vcc.util.DiceBag
import vcc.dnd4e.controller._

class InitiativeDialog(tracker:scala.actors.Actor) extends DialogFrame {

  val initTable=new vcc.util.swing.ProjectionTableModel[InitiativeDialogEntry](InitiativeDialogEntryProjection)
  val table= new EnhancedTable {
    model=initTable
    autoResizeMode=Table.AutoResizeMode.Off
    selection.intervalMode=Table.IntervalMode.Single
    setColumnWidth(0,35)
    setColumnWidth(1,150)
    setColumnWidth(2,35)
    setColumnWidth(3,35)
    setColumnWidth(4,70)
  }	
  title="Roll Initative"
  
  private val groupCheckbox= new CheckBox("Group similar (same name and initiative bonus)")
  contents= new MigPanel("flowy") {
    add( new ScrollPane {contents=table}, "growx,growy")
    add(groupCheckbox)
  }
  minimumSize=new java.awt.Dimension(360,400)
  listenTo(this)
  reactions += {
    case DialogClosed(diag,false) if(diag==this)=>
      val seq=helper.InitiativeRoller.rollInitiative(groupCheckbox.selected,initTable.content.toList)
      tracker ! request.StartCombat(seq)
  }

  // This will build my entries based on TrackerCombatant
  def buildEntryFromCombatant(cmb:vcc.dnd4e.model.TrackerCombatant):InitiativeDialogEntry = {
    new InitiativeDialogEntry(cmb.id,cmb.name,cmb.init,0,false)
  }
  
  override def visible_=(state:Boolean) {
    if(state) {
      var resp=(tracker !? actions.QueryCombatantMap(buildEntryFromCombatant)).asInstanceOf[List[InitiativeDialogEntry]]
      var arr=scala.util.Sorting.stableSort[InitiativeDialogEntry](resp,(a:InitiativeDialogEntry,b:InitiativeDialogEntry)=>{a.id.name < b.id.name})
      initTable.content = arr.toSeq
    }
    super.visible=state
  }
}
