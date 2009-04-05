//$Id$
package vcc.dnd4e.view

import scala.swing._
import scala.swing.event._ 

class CombatantActionPanel(val uia:actors.Actor,val tracker:actors.Actor) extends BorderPanel with view.ContextualView[ViewCombatant] with view.SequenceView[ViewCombatant]{
  minimumSize=new java.awt.Dimension(390,500)
  preferredSize=minimumSize
  val damagePanel=new view.DamageCommandPanel(uia,tracker) 
  val initiativePanel = new view.InitiativePanel(tracker)
  val effectEditorPanel = new view.EffectEditorPanel(tracker)
  
  add(new BoxPanel(Orientation.Vertical) {
    contents+= damagePanel 
    contents+= initiativePanel
    contents+= effectEditorPanel
  },BorderPanel.Position.North)
  
  def changeContext(context:Option[ViewCombatant]) {
    damagePanel.context=context
    initiativePanel.context=context
    effectEditorPanel.context=context
  }
  
  def updateSequence(seq:Seq[ViewCombatant]) {
    initiativePanel.updateSequence(seq)
    effectEditorPanel.updateSequence(seq)
  }
}

