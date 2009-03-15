//$Id$
package vcc.dnd4e.view

import scala.swing._
import scala.swing.event._
import vcc.util.swing._
import scala.actors.Actor

class SequenceTable(uia:Actor) extends ScrollPane with ContextualView[ViewCombatant] with SequenceView[ViewCombatant] {
  //Init
  //val trackerTable=new vcc.util.swing.ProjectionTableModel[ViewCombatant](view.ViewCombatantProjection)
  val table = new RowProjectionTable[ViewCombatant] with CustomRenderedRowProjectionTable[ViewCombatant]{
    val labelFormatter=ViewCombatantTableColorer
    projection = new vcc.util.swing.ProjectionTableModel[ViewCombatant](view.ViewCombatantProjection)
    autoResizeMode=Table.AutoResizeMode.Off
    selection.intervalMode=Table.IntervalMode.Single
    setColumnWidth(0,25)
    setColumnWidth(1,150)
    setColumnWidth(2,70)
    setColumnWidth(3,70)
    setColumnWidth(4,50)
    setColumnWidth(5,70)
    peer.setRowHeight(24)
  }
  
  this.contents=table
  
  var _doingContextChange=false
  
  listenTo(table.selection)
  reactions += {
    case TableRowsSelected(t,rng,b) => 
      var l=table.selection.rows.toSeq
      if(!l.isEmpty && !_doingContextChange) {
        var c=table.content(l(0))
        uia ! actor.SetContext(c.id)
      }	
  }

  def updateSequence(seq:Seq[ViewCombatant]):Unit = { 
    table.content=seq
    table.selection.rows.clear
    if(table.content.isEmpty)
      uia ! actor.SetContext(null)
    else
      table.selection.rows+=0
  }

  def changeContext(ctx:Option[ViewCombatant]) {
    if(ctx.isDefined && !table.content.isEmpty && (ctx.get == table.content(0))) {
      _doingContextChange=true;
      table.selection.rows.clear; 
      table.selection.rows+=0
      _doingContextChange=false                                                                 
    }
  }

  /**
   * Schedule a fireTableRowsUpdated() for a later time. Used to refresh the 
   * values in the table.
   */
  def fireUpdate() {
    SwingHelper.invokeLater(()=>{
      _doingContextChange=true;
      table.projection.fireTableRowsUpdated(0,table.content.length-1)
      _doingContextChange=false                                                                 
    })
  }
  
}
