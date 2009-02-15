//$Id$
package vcc.dnd4e.view

import scala.swing._
import util.swing._

import scala.actors.Actor
import vcc.dnd4e.model.{Effect,Condition}

class EffectViewPanel(tracker:Actor) extends MigPanel("fillx")
  with ContextualView[ViewCombatant]
{
  border= javax.swing.BorderFactory.createTitledBorder("Active Effects")
  
  val sustainButton=new Button("Sustain")
  val cancelButton=new Button("Cancel Effect")
  val effectTable=new EnhancedTable() {
    val entries=new vcc.util.swing.ProjectionTableModel[Effect](tabular.EffectTableProjection)
    
    //TODO: These a dummy
    entries.content=List(Effect('A,Condition.Mark('K,false),true,true,Effect.Duration.EndOfEncounter))
    model=entries
    setColumnWidth(0,30,30,45)
    setColumnWidth(1,50,50,100)
    setColumnWidth(2,200)
  }
  
  add(new ScrollPane(effectTable),"growy,growprio 50,wrap")
  add(sustainButton,"split 3")
  add(cancelButton)
  
  /**
   * Update table according to context
   */
  def changeContext(nctx:Option[ViewCombatant]) {
    
  }
}
