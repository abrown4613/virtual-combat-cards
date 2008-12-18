package vcc.view.dialog


import scala.swing._
import scala.swing.event._
import vcc.util.swing._
import vcc.model.DiceBag

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
      tracker ! vcc.model.actions.StartCombat(l.map(x=>x.id).toSeq)
  }
  
  def roster(seq:Seq[vcc.view.ViewCombatant]) =  {
    var lst=seq map(c=>new InitiativeDialogEntry(c.id,c.name,c.init,0,false))
    var arr=scala.util.Sorting.stableSort[InitiativeDialogEntry](lst,(a:InitiativeDialogEntry,b:InitiativeDialogEntry)=>{a.id.name < b.id.name})
    initTable.content = arr.toSeq
  }
}
