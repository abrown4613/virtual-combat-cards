//$Id$
package vcc.view

import scala.swing._
import scala.swing.event._
import vcc.util.swing._
import scala.actors.Actor

class SequenceTable(uia:Actor) extends ScrollPane with ContextualView[ViewCombatant] with SequenceView[ViewCombatant] {
  //Init
  val trackerTable=new vcc.util.swing.ProjectionTableModel[ViewCombatant](view.ViewCombatantProjection)
  val table = new EnhancedTable {
    model=trackerTable
    autoResizeMode=Table.AutoResizeMode.Off
    selection.intervalMode=Table.IntervalMode.Single
    setColumnWidth(0,20)
    setColumnWidth(1,150)
    setColumnWidth(2,70)
    setColumnWidth(3,70)
    setColumnWidth(4,50)
    setColumnWidth(5,70)
    peer.setRowHeight(24)
    
    override def rendererComponent(isSelected: Boolean,hasFocus: Boolean, row: Int, column: Int): Component= {
      var comp=super.rendererComponent(isSelected,hasFocus, row, column)
      if(comp.peer.isInstanceOf[javax.swing.JLabel]) {
        comp.font=new java.awt.Font(java.awt.Font.SANS_SERIF, java.awt.Font.PLAIN,14)
        if(comp.peer.isInstanceOf[javax.swing.JLabel]) {
          ViewCombatantTableColorer.colorLabel(
            comp.peer.asInstanceOf[javax.swing.JLabel],
            column,isSelected,
            trackerTable.content(row)
          )
        }
      }
      comp
    }
  }
  this.contents=table
  
  var _doingContextChange=false
  
  listenTo(this,table.selection)
  reactions += {
    case TableRowsSelected(t,rng,b) => 
      var l=table.selection.rows.toSeq
      if(!l.isEmpty && !_doingContextChange) {
        var c=trackerTable.content(l(0))
        uia ! vcc.view.actor.SetContext(c.id)
      }	
  }

  def updateSequence(seq:Seq[ViewCombatant]):Unit = { 
    trackerTable.content=seq
    table.selection.rows.clear
    if(trackerTable.content.isEmpty)
      uia ! actor.SetContext(null)
    else
      table.selection.rows+=0
  }
  def changeContext(ctx:Option[ViewCombatant]) {
    if(ctx.isDefined && !trackerTable.content.isEmpty && (ctx.get == trackerTable.content(0))) {
      _doingContextChange=true;
      table.selection.rows.clear; 
      table.selection.rows+=0
      _doingContextChange=false                                                                 
    }
    table.repaint
  }
  
}
