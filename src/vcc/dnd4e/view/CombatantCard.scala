//$Id$
package vcc.dnd4e.view

import scala.swing._
import util.swing._

import scala.actors.Actor

class CombatantCard(tracker:Actor) 
  extends MigPanel("flowy,fill","[300!]","[c,pref!][c,fill]")
  with ContextualView[ViewCombatant]
{

  private val summary=new CombatantSummaryView()
  private val effects=new EffectViewPanel(tracker)
  private val commentPanel= new view.CommentPanel(tracker)
  private val split1=new SplitPane(Orientation.Horizontal,effects,commentPanel)
  
  add(summary,"growx")
  add(split1,"growx")
  
  def changeContext(nctx:Option[ViewCombatant]) {
    summary.context=nctx
    commentPanel.context=nctx
    effects.context=nctx
  }
}
