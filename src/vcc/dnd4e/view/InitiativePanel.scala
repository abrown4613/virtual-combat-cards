//$Id$
package vcc.dnd4e.view

import scala.swing._
import scala.swing.event._
import scala.actors.Actor
import vcc.dnd4e.model._
import vcc.dnd4e.controller._
import util.swing.MigPanel

class InitiativePanel(tracker:Actor) extends MigPanel("flowx","[50%,fill][50%,fill]","") with ContextualView[ViewCombatant] with SequenceView[ViewCombatant]{
  private val startRound_btn=new Button("Start Round")
  startRound_btn.tooltip=("Start round of the first combatant")
  private val endRound_btn=new Button("End Round")
  endRound_btn.tooltip=("End round of the first combatant")
  private val moveUp_btn=new Button("Move Up & Start Round")
  private val delay_btn=new Button("Delay")
  private val ready_btn=new Button("Ready Action")
  private val executeReady_btn=new Button("Execute Ready")

  private var _first:ViewCombatant=null
  
  xLayoutAlignment=java.awt.Component.LEFT_ALIGNMENT;
  contents+=startRound_btn
  add(endRound_btn,"wrap, grow")
  contents+=delay_btn
  add(moveUp_btn,"wrap")
  contents+=executeReady_btn
  contents+=ready_btn
  border=javax.swing.BorderFactory.createTitledBorder("Initiative Actions")
  
  for(x<-contents) { listenTo(x); x.enabled=false}
  
  reactions+= {
    case ButtonClicked(this.startRound_btn) if(_first!=null) => 
      tracker ! request.StartRound(_first.id)
      Thread.sleep(100)
      changeContext(Some(context))
    case ButtonClicked(this.endRound_btn) if(_first!=null) => tracker ! request.EndRound(_first.id)
    case ButtonClicked(this.moveUp_btn) => tracker ! request.MoveUp(context.id)
    case ButtonClicked(this.delay_btn) => tracker ! request.Delay(context.id)
    case ButtonClicked(this.executeReady_btn) => tracker ! request.ExecuteReady(context.id)
    case ButtonClicked(this.ready_btn) => tracker ! request.Ready(context.id)
  }
  
  def changeContext(nctx:Option[ViewCombatant]) = {
    if(nctx.isDefined) {
      var itt=nctx.get.initTracker
      var first=(nctx.get==_first)
      var state=nctx.get.initTracker.state
      if(_first!=null) {
    	startRound_btn.enabled=_first.initTracker.canTransform(true,InitiativeTracker.actions.StartRound)
    	endRound_btn.enabled=_first.initTracker.canTransform(true,InitiativeTracker.actions.Ready)
      } else {
        startRound_btn.enabled=false
        endRound_btn.enabled=false
      }
      ready_btn.enabled=itt.canTransform(first,InitiativeTracker.actions.Ready) 
      //endRound_btn.enabled=itt.canTransform(first,InitiativeTracker.actions.EndRound)
      moveUp_btn.enabled=(
        itt.canTransform(first,InitiativeTracker.actions.MoveUp) && (
          ((state==InitiativeState.Delaying) && (_first.initTracker.state!=InitiativeState.Acting)) ||
          ((state==InitiativeState.Ready) && (_first.initTracker.state==InitiativeState.Acting)) ||
          (state==InitiativeState.Reserve && (_first.initTracker.state!=InitiativeState.Acting))
          ));
      delay_btn.enabled=itt.canTransform(first,InitiativeTracker.actions.StartRound)
      executeReady_btn.enabled=(itt.canTransform(first,InitiativeTracker.actions.ExecuteReady)&& _first.initTracker.state==InitiativeState.Acting)
    } else {
      for(x<-this.contents) { x.enabled=false; }
    }
  }
  def updateSequence(seq:Seq[ViewCombatant]):Unit= {
    _first=if(seq.isEmpty)null else seq(0)
  }
}
