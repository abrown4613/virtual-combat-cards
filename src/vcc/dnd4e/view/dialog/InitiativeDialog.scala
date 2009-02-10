//$Id$
package vcc.dnd4e.view.dialog


import scala.swing._
import scala.swing.event._
import vcc.util.swing._

import vcc.model.DiceBag
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
  contents=new ScrollPane {
    contents=table
  }
  minimumSize=new java.awt.Dimension(360,400)
  listenTo(this)
  reactions += {
    case DialogClosed(diag,false) if(diag==this)=>
      var l=initTable.content.filter(x=> !x.reserve).toArray
      l.foreach(x=>if(x.roll<=0) x.roll=DiceBag.D(20))
      scala.util.Sorting.quickSort[InitiativeDialogEntry](l)(x=>x)
      tracker ! actions.StartCombat(l.map(x=>x.id).toSeq)
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
