package vcc.view.dialog


import scala.swing._
import scala.swing.event._
import vcc.util.swing._
import vcc.model.DiceBag

class InitiativeDialog(tracker:scala.actors.Actor) extends DialogFrame {

  val initTable=new vcc.util.swing.ProjectionTableModel[InitiativeDialogEntry](InitiativeDialogEntryProjection)
  val table= new EnhancedTable {
    model=initTable
  }	
  title="Roll Initative"
  contents=new ScrollPane {
    contents=table
  }
  minimumSize=new java.awt.Dimension(300,400)
  listenTo(this)
  reactions += {
    case DialogClosed(diag,false) if(diag==this)=>
      //println("***** ROLL INITIATIVE ******")
      var l=initTable.content.filter(x=> !x.reserve).toArray
      //println(l)
      l.foreach(x=>if(x.roll<=0) x.roll=DiceBag.D(20))
      //println("After rolling\n"+(l.toString))
      scala.util.Sorting.quickSort[InitiativeDialogEntry](l)(x=>x)
      //println("After sort\n"+(l.toString))
      //println("Sequence of ID"+l.map(x=>x.id).toSeq)
      tracker ! vcc.model.actions.StartCombat(l.map(x=>x.id).toSeq)
  }
  
  def roster(seq:Seq[vcc.view.ViewCombatant]) =  {
    initTable.content = seq map(c=>new InitiativeDialogEntry(c.id,c.name,c.init,0,false))
  }
}
