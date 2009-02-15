//$Id$
package vcc.dnd4e.view

import scala.swing._
import scala.swing.event._ 

class CombatantActionPanel(val tracker:actors.Actor) extends BorderPanel with view.ContextualView[ViewCombatant] with view.SequenceView[ViewCombatant]{
  minimumSize=new java.awt.Dimension(400,700)
  preferredSize=minimumSize
  val damagePanel=new view.DamageCommandPanel(tracker) 
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

