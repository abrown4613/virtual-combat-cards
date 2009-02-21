//$Id$
package vcc.dnd4e.view

import scala.swing._
import util.swing._

import scala.actors.Actor
import vcc.dnd4e.model.{Effect,Condition}
import vcc.dnd4e.controller.request

class EffectViewPanel(tracker:Actor) extends MigPanel("fillx") with ContextualView[ViewCombatant]
{
  border= javax.swing.BorderFactory.createTitledBorder("Active Effects")
  
  private val sustainButton=new Button("Sustain")
  sustainButton.enabled=false
  private val cancelButton=new Button("Cancel Effect")
  cancelButton.enabled=false
  
  val effectModel=new vcc.util.swing.ProjectionTableModel[Effect](tabular.EffectTableProjection)
  val effectTable=new EnhancedTable() {
    autoResizeMode=Table.AutoResizeMode.Off
    selection.intervalMode=Table.IntervalMode.Single
    model=effectModel
    setColumnWidth(0,25)
    setColumnWidth(1,10)
    setColumnWidth(2,50,50,100)
    setColumnWidth(3,200)
  }
  
  add(new ScrollPane(effectTable),"growy,growprio 50,wrap")
  add(sustainButton,"split 3")
  add(cancelButton)
  
  listenTo(effectTable.selection)
  listenTo(sustainButton,cancelButton)
  
  reactions += {
    case event.ButtonClicked(this.sustainButton) =>
    case event.ButtonClicked(this.cancelButton) =>
      tracker ! request.CancelEffect(context.id,effectTable.selection.rows.toSeq(0))
    case event.TableRowsSelected(this.effectTable,rng,opt) =>
      val sel=effectTable.selection.rows
      if(sel.isEmpty) {
        cancelButton.enabled=false
        sustainButton.enabled=false
      } else {
        val eff=effectModel.content(sel.toSeq(0))
        sustainButton.enabled=eff.sustainable
        cancelButton.enabled=true
      }
  }
  
  /**
   * Update table according to context
   */
  def changeContext(nctx:Option[ViewCombatant]) {
    nctx match {
      case Some(c) => 
        SwingHelper.invokeLater(()=>{
          effectModel.content=c.effects
        })
      case None =>
    }
  }
}
