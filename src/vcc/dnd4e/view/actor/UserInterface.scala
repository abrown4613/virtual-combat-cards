//$Id$
package vcc.dnd4e.view.actor

import vcc.dnd4e.model.{HealthTracker,InitiativeTracker}
case class Combatant(vc:ViewCombatant)
case class SetHealth(id:Symbol, hts:HealthTracker)
case class SetInitiative(id:Symbol, init:InitiativeTracker)
case class SetSequence(seq:Seq[Symbol]) extends vcc.controller.transaction.ChangeNotification
case class SetInformation(id:Symbol,text:String)
case class SetContext(id:Symbol)
case class GoToFirst()
case class SetOption(opt:Symbol,state:Boolean)
case class ClearSequence()
case class SetTip(tip:String)

import scala.actors.Actor._
import scala.actors.Actor

import vcc.dnd4e.controller.response

class UserInterface(tracker:Actor) extends Actor {
  
  type T=ViewCombatant
  
  private var _hidedead=false
  private var _seq:Seq[T]=Nil
  private var _ctx:Option[T]=None
  private var seqAware:List[SequenceView[T]]=Nil
  private var ctxAware:List[ContextualView[T]]=Nil
  private var statusBar:StatusBar=null
  private val _map=scala.collection.mutable.Map.empty[Symbol,T]
  object InMap {
    def unapply(id:Symbol):Option[T] = if(id!=null && _map.contains(id)) Some(_map(id)) else None
  }
  
  /**
   * Signal all context objects, this has to be done in the Swing Thread to avoid 
   * race conditions.
   */
  protected def signalContext(o:Option[T]) {
    util.swing.SwingHelper.invokeLater(()=> {
      for(x<-ctxAware) x.context=(o)
    })
  }
  
  protected def signalSequence(seq:Seq[T]) {
    util.swing.SwingHelper.invokeLater(()=> {
      for(x<-seqAware) x.updateSequence(seq)
    })
  }
  
  def flush():Unit = {
    _map.clear
    signalContext(None)
    signalSequence(Nil)
  }
  
  private var _first:ViewCombatant=null;
  
  def addSequenceListener(seq:SequenceView[T]) { seqAware=seq :: seqAware }
  def addContextListener(ctx:ContextualView[T]) { ctxAware=ctx :: ctxAware }
  
  def setStatusBar(sb: StatusBar) { 
    statusBar=sb
  }
  
  /**
   * This is to update sequence table, it's kind of a hack.
   */
  def updateSequenceTable() {
    ctxAware.foreach(x=> 
      x match {
        case x:SequenceTable => x.fireUpdate
        case _ =>
      })
  }
  
  def act() {
    loop {
      react {
        case SetContext(null) => _ctx=None; signalContext(None);
        case SetContext(InMap(o)) => _ctx=Some(o); signalContext(_ctx);
        case Combatant(c) => _map(c.id)=c
        case SetHealth(InMap(o), hts)=> 
          o.health=hts
          if(_ctx == Some(o)) signalContext(Some(o))
          updateSequenceTable()
        case SetInitiative(InMap(o), inits)=>
          o.initTracker=inits
          if(_ctx == Some(o)) signalContext(Some(o))
          updateSequenceTable()
        case SetInformation(InMap(o),text)=>
          o.info=text
          if(_ctx == Some(o)) signalContext(Some(o))
          
        case ClearSequence() => 
          _map.clear
          _ctx=None
          signalContext(_ctx)
          signalSequence(Nil)
        case SetSequence(seq)=>
          var l=seq.filter(_map.contains(_)).map(_map(_))
          _seq=l // Save all elements irrespective of health, then filter health and proppagate
          if(_hidedead) l=l.filter(x=>x.health.status!=HealthTracker.Status.Dead)
          _first=if(l.isEmpty) null else l(0)
          signalSequence(l)
        case GoToFirst() =>
          _ctx=Some(_first)
          signalContext(_ctx)
        
        //Update effect list and notify
        case response.UpdateEffects(InMap(o),el)=>
          o.effects=el
          if(_ctx == Some(o)) signalContext(Some(o))

        //Set view options
        case SetOption('HIDEDEAD,state) => 
          _hidedead=state
          this ! SetSequence(_seq.map(x=>x.id))
          
        case SetTip(str) =>
          statusBar.setTipText(str)
          
        case s => println("UIA: Unhandled message:" + s)
      }
    }
  }
}
