//$Id$
package vcc.dnd4e.view

import scala.swing._
import util.swing._

import scala.actors.Actor
import vcc.dnd4e.model.Effect

class EffectViewPanel(tracker:Actor) extends MigPanel("fillx")
  with ContextualView[ViewCombatant]
{
  border= javax.swing.BorderFactory.createTitledBorder("Active Effects")

  val sustainButton=new Button("Sustain")
  val cancelButton=new Button("Cancel Effect")
  val effectTable=new AlternativeTable() {
    val entries=new vcc.util.swing.ProjectionTableModel[Effect](tabular.EffectTableProjection)
    
    entries.content=List(Effect('A,"Some","Other"))
    model=entries
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
