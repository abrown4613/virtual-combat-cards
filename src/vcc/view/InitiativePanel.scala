package vcc.view

import scala.swing._
import scala.swing.event._
import scala.actors.Actor
import vcc.model._

class InitiativePanel(tracker:Actor) extends GridPanel(3,2) with ContextualView[ViewCombatant] with SequenceView[ViewCombatant]{
  val startRound_btn=new Button("Start Round")
  val endRound_btn=new Button("End Round")
  val moveUp_btn=new Button("Move Up & Start Round")
  val delay_btn=new Button("Delay")
  val ready_btn=new Button("Ready Action")
  val executeReady_btn=new Button("Execute Ready")

  private var _first:ViewCombatant=null
  
  xLayoutAlignment=java.awt.Component.LEFT_ALIGNMENT;
  contents+=startRound_btn
  contents+=endRound_btn
  contents+=moveUp_btn
  contents+=delay_btn
  contents+=executeReady_btn
  contents+=ready_btn
  border=javax.swing.BorderFactory.createTitledBorder("Initiative Actions")
  
  for(x<-contents) { listenTo(x); x.enabled=false}
  
  reactions+= {
    case ButtonClicked(this.startRound_btn) => tracker ! actions.StartRound(context.id)
    case ButtonClicked(this.endRound_btn) => tracker ! actions.EndRound(context.id)
    case ButtonClicked(this.moveUp_btn) => tracker ! actions.MoveUp(context.id)
    case ButtonClicked(this.delay_btn) => tracker ! actions.Delay(context.id)
    case ButtonClicked(this.executeReady_btn) => tracker ! actions.ExecuteReady(context.id)
    case ButtonClicked(this.ready_btn) => tracker ! actions.Ready(context.id)
  }
  
  def changeContext(nctx:Option[ViewCombatant]) = {
    if(nctx.isDefined) {
      var itt=nctx.get.initTracker.transform
      var first=(nctx.get==_first)
      var state=nctx.get.initTracker.state
      startRound_btn.enabled=itt.isDefinedAt(first,InitiativeTracker.actions.StartRound)
      ready_btn.enabled=itt.isDefinedAt(first,InitiativeTracker.actions.Ready)
      endRound_btn.enabled=itt.isDefinedAt(first,InitiativeTracker.actions.EndRound)
      moveUp_btn.enabled=(
        itt.isDefinedAt(first,InitiativeTracker.actions.MoveUp) && (
          ((state==InitiativeState.Delaying) && (_first.initTracker.state!=InitiativeState.Acting)) ||
          ((state==InitiativeState.Ready) && (_first.initTracker.state==InitiativeState.Acting)) ||
          (state==InitiativeState.Reserve && (_first.initTracker.state!=InitiativeState.Acting))
          ));
      delay_btn.enabled=itt.isDefinedAt(first,InitiativeTracker.actions.Delay)
      executeReady_btn.enabled=itt.isDefinedAt(first,InitiativeTracker.actions.ExecuteReady)
    } else {
      for(x<-this.contents) { x.enabled=false;println(x)}
    }
  }
  def updateSequence(seq:Seq[ViewCombatant]):Unit= {
    _first=if(seq.isEmpty)null else seq(0)
  }
}
