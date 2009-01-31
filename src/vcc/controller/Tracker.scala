//$Id$
package vcc.controller

import scala.actors.Actor
import scala.actors.Actor.loop

import vcc.controller.transaction._

import vcc.model._

case class CombatantUpdate(comb:Symbol, obj:Any) extends ChangeNotification
case class RosterUpdate(obj:Map[Symbol,TrackerCombatant]) extends ChangeNotification


class TrackerCombatant(val id:Symbol,val name:String,val hp:Int,val init:Int,ctype:CombatantType.Value) {
  val health:HealthTracker= ctype match {
    case CombatantType.Minion => new MinionHealthTracker
    case CombatantType.Monster => new MonsterHealthTracker(hp)
    case CombatantType.Character => new CharacterHealthTracker(hp)
  }
  var info:String=""
  var it=new Undoable[InitiativeTracker](InitiativeTracker(0,InitiativeState.Reserve),(uv)=>{CombatantUpdate(id,uv.value)})
  var defense:DefenseBlock=null
}

class Tracker() extends Actor with TransactionChangePublisher {
  
  private var uia:Actor=null
  private var _coord:Coordinator =null
  private val _idgen= new IDGenerator(1,50)
  
  private var _initSeq=new CombatSequencer
  private var _undo_map:Undoable[Map[Symbol,TrackerCombatant]]=
    new Undoable(Map.empty[Symbol,TrackerCombatant],un=>RosterUpdate(un.value))
  
  private def _map_=(m:Map[Symbol,TrackerCombatant])(implicit trans:Transaction)= _undo_map.value=m
  private def _map = _undo_map.value
  
  private val _tlog= new TransactionLog[actions.TransactionalAction]()
  
  class ComposedAction(val name:String) extends actions.TransactionalAction {
    private var acts:List[actions.TransactionalAction] =Nil
    private def add(act:actions.TransactionalAction) {
      acts=act::acts
    }
    def description():String= name
    lazy val transaction=new Transaction()
  }
  
  private var _composedAction: ComposedAction = null
  
  private object InMap {
    def unapply(id:Symbol):Option[TrackerCombatant]= if(_map.contains(id)) Some(_map(id)) else None
  }
  
  def startTransaction() = if(_composedAction!=null) _composedAction.transaction else new Transaction()
  
  def closeTransaction(action:actions.TransactionalAction, trans:Transaction) {
    if(_composedAction==null || (_composedAction eq action)) {
      //Need to close composed transcation or a simple transaction
      trans.commit(this)
      if(!trans.isEmpty) {
        _tlog.store(action,trans)
        println("TLOG["+ _tlog.length +"] Added transaction"+ _tlog.previousTransctionDescription)
      }
    }
  }
  
  
  /**
   * Publish changes to the observers
   */
  def publishChange(changes:Seq[ChangeNotification]) {
    changes.foreach {
      case CombatantUpdate(comb, s:InitiativeTracker) => uia ! vcc.view.actor.SetInitiative(comb,s)
      case RosterUpdate(map) => enumerate(map)
      case s:vcc.view.actor.SetSequence => uia ! s
    }
  }
  
  
  def act()={
    loop {
      react {
        case actions.AddObserver(obs) => 
          uia=obs
          //TODO: This is a wrong approach need to add as many observer as possible
        case actions.SetCoordinator(coord) => 
          this._coord=coord
        case ta:actions.TransactionalAction => 
          val trans=startTransaction()
          processActions(ta)(trans)
          closeTransaction(ta,trans)
        case actions.StartTransaction(tname) =>
          if(_composedAction==null)
            _composedAction=new ComposedAction(tname)
          else
            throw new Exception("Cant nest transaction")
        case actions.EndTransaction(tname) => 
          if(_composedAction!=null) {
            if(_composedAction.name==tname) { 
              closeTransaction(_composedAction,_composedAction.transaction)
              _composedAction=null
            } else throw new Exception("Tranction name mismatch, expected"+_composedAction.name+" found "+tname)
          } else throw new Exception("Not in compound transaction")
        case qa:actions.QueryAction if(processQuery.isDefinedAt(qa)) =>
          processQuery(qa)
        case actions.Undo() =>
          try {
            _tlog.rollback(this)
          } catch { case s:TransactionLogOutOfBounds => }
        case actions.Redo() =>
          try {
            _tlog.rollforward(this)
          } catch { case s:TransactionLogOutOfBounds => }
        case s=>println("Error: Tracker receive:"+s)
      }
    }
  }
  
  private def processActions(action:actions.TransactionalAction)(implicit trans:Transaction) {
    action match {
    case actions.AddCombatant(template)=>
      var id:Symbol=if(template.id==null) _idgen.first() else {
        var s=Symbol(template.id)
        if(_idgen.contains(s)) _idgen.removeFromPool(s) // To make sure we don't get doubles
        s
      }
      var nc=new TrackerCombatant(id,template.name,template.hp,template.init,template.ctype)
      nc.defense=template.defense
      if(_map.contains(nc.id)) {
        // It's an old combatant salvage old heath and Initiative
        //FIXME: This is a Hack, health is not well implemented
        nc.health._currhp =_map(nc.id).health._currhp
        nc.health._temphp =_map(nc.id).health._temphp
        nc.health._deathStrikes =_map(nc.id).health._deathStrikes
        nc.it =_map(nc.id).it
      } else {
        _initSeq add id
      }
      _map = _map + (id -> nc)
    case actions.StartCombat(seq) =>
      for(x<-seq) {
        if(_map.contains(x)) {
          var c=_map(x)
          _initSeq.moveDown(c.id)
          c.it.value=InitiativeTracker(0,InitiativeState.Waiting)
        }
      } 
    case actions.ClearCombatants(all) =>
      var current=_map.keySet.toList
      if(all) {
        _map=Map.empty[Symbol,TrackerCombatant]
      } else {
        _map=_map.filter(p=>p._2.health.isInstanceOf[CharacterHealthTracker])
      }
      var removed=current -- _map.keySet.toList
      for(x <- removed) {
        _idgen.returnToPool(x)
      }
      _initSeq.removeFromSequence(removed)
    case actions.EndCombat() => {
      for(p<-_map) {
        var c=p._2
        c.it.value=InitiativeTracker(0,InitiativeState.Reserve)
      }
    }
    
    // HEALTH Tracking
    case actions.ApplyDamage(InMap(c),amnt) =>
      c.health.applyDamage(amnt)
      //log ! c.name + " took " + amnt + " points of damage"
      uia ! vcc.view.actor.SetHealth(c.id,c.health.getSummary)
    case actions.HealDamage(InMap(c),amnt) =>
      c.health.heal(amnt)
      //log ! c.name + " healed " + amnt + " points of damage"
      uia ! vcc.view.actor.SetHealth(c.id,c.health.getSummary)
    case actions.SetTemporaryHP(InMap(c),amnt) =>
      c.health.setTemporaryHitpoint(amnt)
      //log ! c.name + " received " + amnt + " of temporary hit points"
      uia ! vcc.view.actor.SetHealth(c.id,c.health.getSummary)
    case actions.FailDeathSave(InMap(c)) =>
      c.health.failDeathSave()
      //log ! c.name + " failed save versus death"
      uia ! vcc.view.actor.SetHealth(c.id,c.health.getSummary)
    case actions.SetComment(InMap(c),text)=>
      c.info=text
      uia ! vcc.view.actor.SetInformation(c.id,c.info)
      
      // INITIATIVE TRACKING  
    case actions.MoveUp(InMap(c)) => 
      this.changeSequence(c,InitiativeTracker.actions.MoveUp)
    case actions.StartRound(InMap(c)) =>
      this.changeSequence(c,InitiativeTracker.actions.StartRound)
    case actions.EndRound(InMap(c)) =>
      this.changeSequence(c,InitiativeTracker.actions.EndRound)
    case actions.Delay(InMap(c)) =>
      this.changeSequence(c,InitiativeTracker.actions.Delay)
    case actions.Ready(InMap(c)) => 
      this.changeSequence(c,InitiativeTracker.actions.Ready)
    case actions.ExecuteReady(InMap(c)) =>
      this.changeSequence(c,InitiativeTracker.actions.ExecuteReady)
    }
  }	
  
  private def enumerate(map:Map[Symbol,TrackerCombatant]) {
    val peer = uia
    peer ! vcc.view.actor.ClearSequence()
    for(x<-_map.map(_._2)) { 
      peer ! vcc.view.actor.Combatant(vcc.view.ViewCombatant(x.id,x.name,x.hp,x.init,x.defense))
      peer ! vcc.view.actor.SetHealth(x.id,x.health.getSummary)
    }
    peer ! vcc.view.actor.SetSequence(_initSeq.sequence)
  }
  
  val processQuery:PartialFunction[actions.QueryAction,Unit]= {
    case actions.QueryCombatantMap(func) =>
      reply(_map.map(x=>func(x._2)).toList)
    case actions.Enumerate()=> enumerate(_map)
  }		
  
  private def advanceToNext()(implicit trans:Transaction) {
    // Auto advance dead guys
    while(_map(_initSeq.sequence.head).health.status==HealthStatus.Dead) {
      var dcmb=_map(_initSeq.sequence.head)
      var dit=dcmb.it
      dit.value=dit.value.transform(true,InitiativeTracker.actions.StartRound)
      dit.value=dit.value.transform(true,InitiativeTracker.actions.EndRound)
      dcmb.it=dit
      _initSeq.rotate
    }
  }
  
  def changeSequence(cmb:TrackerCombatant,action:InitiativeTracker.actions.Value)(implicit trans:Transaction) {
    var itt=cmb.it.value.transform
    var firstp=_map(_initSeq.sequence.head).id==cmb.id
    if(itt.isDefinedAt(firstp,action)) {
      action match {
        case InitiativeTracker.actions.Delay => 
          _initSeq.moveDown(cmb.id)
          advanceToNext
        case InitiativeTracker.actions.ExecuteReady => 
          _initSeq.moveDown(cmb.id)
        case InitiativeTracker.actions.EndRound =>
          // When delaying is up, end turn is end of previous
          if(cmb.it.value.state!=InitiativeState.Delaying) { 
            _initSeq.rotate
            advanceToNext()
          }
        case InitiativeTracker.actions.Ready => 
          _initSeq.moveDown(cmb.id)
          advanceToNext
        case InitiativeTracker.actions.MoveUp => 
          _initSeq.moveUp(cmb.id)
        case _ =>
      }
      cmb.it.value=itt(firstp,action)
    }
  }
}