//$Id$
package vcc.dnd4e.view

import scala.swing._
import scala.swing.event._
import scala.actors.Actor
import vcc.dnd4e.model._
import vcc.dnd4e.controller._
import util.swing.MigPanel
import util.swing.ExplicitModelComboBox
import util.swing.ContainterComboBoxModel

class InitiativePanel(tracker:Actor) extends MigPanel("flowx","[50%,fill][50%,fill]","") with ContextualView[ViewCombatant] with SequenceView[ViewCombatant]{
  private val startRound_btn=new Button("Start Round")
  startRound_btn.tooltip=("Start round of the first combatant")
  private val endRound_btn=new Button("End Round")
  endRound_btn.tooltip=("End round of the first combatant")
  private val moveUp_btn=new Button("Move Up & Start Round")
  private val delay_btn=new Button("Delay")
  private val ready_btn=new Button("Ready Action")
  private val executeReady_btn=new Button("Execute Ready")
  private val moveBefore_btn=new Button("Move")
  private val moveLabel=new Label("Move select combatant before:")
  moveLabel.horizontalAlignment= scala.swing.Alignment.Right
  private val candidateBefore = new ContainterComboBoxModel[String](Nil)
  private val before_Combo=new ExplicitModelComboBox[String](candidateBefore)
  
  moveBefore_btn.tooltip = "Move select combatant to a position before the combatant selected on the combo box to the left"

  private var _first:ViewCombatant=null
  private var _seq:List[ViewCombatant]=Nil
  
  xLayoutAlignment=java.awt.Component.LEFT_ALIGNMENT;
  contents+=startRound_btn
  add(endRound_btn,"wrap, grow")
  contents+=delay_btn
  add(moveUp_btn,"wrap")
  contents+=executeReady_btn
  add(ready_btn,"wrap")
  add(moveLabel,"align right")
  add(before_Combo,"split 2")
  add(moveBefore_btn)
  border=javax.swing.BorderFactory.createTitledBorder("Initiative Actions")
  
  for(x<-contents) { listenTo(x); x.enabled= x.isInstanceOf[Label] }
  
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
    case ButtonClicked(this.moveBefore_btn) if(before_Combo.item!=null)=>
      tracker ! request.MoveBefore(context.id,Symbol(before_Combo.item))
  }
  
  def changeContext(nctx:Option[ViewCombatant]) = {
    if(nctx.isDefined) {
      var itt=nctx.get.initTracker
      var first=(nctx.get==_first)
      var state=nctx.get.initTracker.state
      if(_first!=null) {
    	startRound_btn.enabled=_first.initTracker.canTransform(true,InitiativeTracker.actions.StartRound)
    	endRound_btn.enabled=_first.initTracker.canTransform(true,InitiativeTracker.actions.EndRound)
      } else {
        startRound_btn.enabled=false
        endRound_btn.enabled=false
      }
      moveBefore_btn.enabled = nctx.get.initTracker.state!=InitiativeState.Acting && nctx.get.initTracker.state!=InitiativeState.Reserve
      before_Combo.enabled=moveBefore_btn.enabled
      moveLabel.enabled=moveBefore_btn.enabled
      ready_btn.enabled=itt.canTransform(first,InitiativeTracker.actions.Ready)
      moveLabel.text="Move ["+ nctx.get.id.name  +"] before:"
      
      //endRound_btn.enabled=itt.canTransform(first,InitiativeTracker.actions.EndRound)
      moveUp_btn.enabled=(
        itt.canTransform(first,InitiativeTracker.actions.MoveUp) && (
          ((state==InitiativeState.Delaying) && (_first.initTracker.state!=InitiativeState.Acting)) ||
          ((state==InitiativeState.Ready) && (_first.initTracker.state==InitiativeState.Acting)) ||
          (state==InitiativeState.Reserve && (_first.initTracker.state!=InitiativeState.Acting))
          ));
      delay_btn.enabled=itt.canTransform(first,InitiativeTracker.actions.StartRound)
      executeReady_btn.enabled=(itt.canTransform(first,InitiativeTracker.actions.ExecuteReady)&& _first.initTracker.state==InitiativeState.Acting)
      
      // Get possible combatant to move before, exclude acting and reserver combatants
      val before=_seq.filter {c=> c.initTracker.state!=InitiativeState.Acting && c.initTracker.state!=InitiativeState.Reserve }.map{c=>c.id.name}
      candidateBefore.contents = before
      before_Combo.selection.index = -1
    } else {
      moveLabel.text="Move select combatant before:"
      for(x<-this.contents) { x.enabled=false; }
    }
  }
  def updateSequence(seq:Seq[ViewCombatant]):Unit= {
    _first=if(seq.isEmpty)null else seq(0)
    _seq=seq.toList
  }
}
