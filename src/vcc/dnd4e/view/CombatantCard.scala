//$Id$
package vcc.dnd4e.view

import scala.swing._
import util.swing._

import scala.actors.Actor

class CombatantCard(tracker:Actor) 
  extends MigPanel("flowy,fillx","[300!]","[c,pref!][c,grow 25,fill][c,grow 75,fill]")
  with ContextualView[ViewCombatant]
{

  val summary=new CombatantSummaryView()
  val effects=new EffectViewPanel(tracker)
  val commentPanel= new view.CommentPanel(tracker)
  
  add(summary,"growx")
  add(effects,"growx")
  add(commentPanel,"growx,hmin 120px")
  
  def changeContext(nctx:Option[ViewCombatant]) {
    summary.context=nctx
    commentPanel.context=nctx
    effects.context=nctx
  }
}
