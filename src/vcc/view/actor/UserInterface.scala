//$Id$
package vcc.view.actor

case class Combatant(vc:ViewCombatant)
case class SetHealth(id:Symbol, hts:vcc.model.HealthTracker)
case class SetInitiative(id:Symbol, init:vcc.model.InitiativeTracker)
case class SetSequence(seq:Seq[Symbol]) extends vcc.controller.transaction.ChangeNotification
case class SetInformation(id:Symbol,text:String)
case class SetContext(id:Symbol)
case class GoToFirst()
case class SetOption(opt:Symbol,state:Boolean)
case class ClearSequence()

import scala.actors.Actor._
import scala.actors.Actor

class UserInterface(tracker:Actor) extends Actor {
  
  type T=ViewCombatant
  
  private var _hidedead=false
  private var _seq:Seq[T]=Nil
  private var _ctx:Option[T]=None
  private var seqAware:List[SequenceView[T]]=Nil
  private var ctxAware:List[ContextualView[T]]=Nil
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
  
  protected def signalSequence(seq:Seq[T]) {for(x<-seqAware) x.updateSequence(seq)}
  
  def flush():Unit = {
    _map.clear
    signalContext(None)
    signalSequence(Nil)
  }
  
  private var _first:ViewCombatant=null;
  
  def addSequenceListener(seq:SequenceView[T]) { seqAware=seq :: seqAware }
  def addContextListner(ctx:ContextualView[T]) { ctxAware=ctx :: ctxAware }
  
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
          if(_hidedead) l=l.filter(x=>x.health.status!=vcc.model.HealthStatus.Dead)
          _first=if(l.isEmpty) null else l(0)
          signalSequence(l)
        case GoToFirst() =>
          _ctx=Some(_first)
          signalContext(_ctx)

        //Set view options
        case SetOption('HIDEDEAD,state) => 
          _hidedead=state
          this ! SetSequence(_seq.map(x=>x.id))
        case s => println("Unhandled message:" + s)
      }
    }
  }
}
