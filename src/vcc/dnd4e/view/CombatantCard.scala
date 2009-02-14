//$Id$
package vcc.dnd4e.view

import scala.swing._
import util.swing._

import scala.actors.Actor

class CombatantCard(tracker:Actor) 
  extends MigPanel("flowy,fillx,debug","[300!]","[c,pref!][c,grow 50,fill][c,grow 50,fill]")
  with ContextualView[ViewCombatant]
{

  val summary=new CombatantSummaryView()
  val effects=new EffectViewPanel(tracker)
  val commentPanel= new view.CommentPanel(tracker)
  
  add(summary,"growx")
  add(effects,"growx")
  add(commentPanel,"growx")
  
  
  def changeContext(nctx:Option[ViewCombatant]) {
    summary.context=nctx
    commentPanel.context=nctx
  }
}
