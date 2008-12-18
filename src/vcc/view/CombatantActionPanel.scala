package vcc.view

import scala.swing._
import scala.swing.event._ 

class CombatantActionPanel(val tracker:actors.Actor) extends BorderPanel with view.ContextualView[ViewCombatant] with view.SequenceView[ViewCombatant]{
  minimumSize=new java.awt.Dimension(400,500)
  preferredSize=minimumSize
  val damagePanel=new view.DamageCommandPanel(tracker) 
  val commentPanel= new view.CommentPanel(tracker)
  val combatantPanel = new view.CombatantSummaryView
  val initiativePanel = new view.InitiativePanel(tracker)
  
  add(new BoxPanel(Orientation.Vertical) {
    contents+= combatantPanel
    contents+= damagePanel 
    contents+= initiativePanel
  },BorderPanel.Position.North)
  add(commentPanel, BorderPanel.Position.Center)
  
  def changeContext(context:Option[ViewCombatant]) {
    damagePanel.context=context
    commentPanel.context=context
    combatantPanel.context=context
    initiativePanel.context=context
  }
  def updateSequence(seq:Seq[ViewCombatant]) {
    initiativePanel.updateSequence(seq)
  }
}

