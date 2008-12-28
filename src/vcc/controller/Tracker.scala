//$Id$
package vcc.controller

import scala.actors.Actor
import scala.actors.Actor.loop

import vcc.model._

class TrackerCombatant(val id:Symbol,val name:String,val hp:Int,val init:Int,ctype:CombatantType.Value) {
  val health:HealthTracker= ctype match {
    case CombatantType.Minion => new MinionHealthTracker
    case CombatantType.Monster => new MonsterHealthTracker(hp)
    case CombatantType.Character => new CharacterHealthTracker(hp)
  }
  var info:String=""
  var it=InitiativeTracker(0,InitiativeState.Reserve)
}

class Tracker(log:Actor) extends Actor {
  
  private var uia:Actor=null
  
  def setUserInterfaceActor(a:Actor) { uia=a}
  private var _initSeq=new CombatSequencer[Symbol]
  private var _map=Map.empty[Symbol,TrackerCombatant]
  
  private object InMap {
    def unapply(id:Symbol):Option[TrackerCombatant]= if(_map.contains(id)) Some(_map(id)) else None
  }
  
  def act()={
    loop {
      react {
        case actions.AddCombatant(id,template)=>
          var nc=new TrackerCombatant(id,template.name,template.hp,template.init,template.ctype)
          _initSeq add id
          _map=_map + (id -> nc)
        case actions.Enumerate(peer)=>
          peer ! vcc.view.actor.ClearSequence()
          for(x<-_map.map(_._2)) { 
            peer ! vcc.view.actor.Combatant(vcc.view.ViewCombatant(x.id,x.name,x.hp,x.init))
            peer ! vcc.view.actor.SetInitiative(x.id,x.it)
            peer ! vcc.view.actor.SetHealth(x.id,x.health.getSummary)
          }
          peer ! vcc.view.actor.SetSequence(_initSeq.sequence)
        case actions.StartCombat(seq) =>
          for(x<-seq) {
            if(_map.contains(x)) {
              var c=_map(x)
              _initSeq.moveDown(c.id)
              c.it=InitiativeTracker(0,InitiativeState.Waiting)
              uia ! vcc.view.actor.SetInitiative(c.id,c.it)
            }
          } 
          uia ! vcc.view.actor.SetSequence(_initSeq.sequence)
        case actions.ClearCombatants(all) =>
          if(all) {
            _map=Map.empty[Symbol,TrackerCombatant]
            _initSeq.clear
          } else {
            _map=_map.filter(p=>p._2.health.isInstanceOf[CharacterHealthTracker])
            _initSeq.clear
            for(c<-_map) _initSeq.add(c._2.id)
          }
        case actions.EndCombat() => {
          for(p<-_map) {
            var c=p._2
            c.it=InitiativeTracker(0,InitiativeState.Reserve)
            uia ! vcc.view.actor.SetInitiative(c.id,c.it)
          }
        }
          
        // HEALTH Tracking
        case actions.ApplyDamage(InMap(c),amnt) =>
          c.health.applyDamage(amnt)
          log ! c.name + " took " + amnt + " points of damage"
          uia ! vcc.view.actor.SetHealth(c.id,c.health.getSummary)
        case actions.HealDamage(InMap(c),amnt) =>
          c.health.heal(amnt)
          log ! c.name + " healed " + amnt + " points of damage"
          uia ! vcc.view.actor.SetHealth(c.id,c.health.getSummary)
        case actions.SetTemporaryHP(InMap(c),amnt) =>
          c.health.setTemporaryHitpoint(amnt)
          log ! c.name + " received " + amnt + " of temporary hit points"
          uia ! vcc.view.actor.SetHealth(c.id,c.health.getSummary)
        case actions.FailDeathSave(InMap(c)) =>
          c.health.failDeathSave()
          log ! c.name + " failed save versus death"
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
          
        case actions.QueryCombatantMap(func) =>
          reply(_map.map(x=>func(x._2)).toList)
          
        case s=>println("Tracker receive:"+s)
      }
    }
  }
  
  private def advanceToNext() {
    // Auto advance dead guys
    while(_map(_initSeq.sequence.head).health.status==HealthStatus.Dead) {
      var dcmb=_map(_initSeq.sequence.head)
      var dit=dcmb.it
      dit=dit.transform(true,InitiativeTracker.actions.StartRound)
      dit=dit.transform(true,InitiativeTracker.actions.EndRound)
      dcmb.it=dit
      _initSeq.rotate
      uia ! vcc.view.actor.SetInitiative(dcmb.id,dcmb.it)
    }
  }
  def changeSequence(cmb:TrackerCombatant,action:InitiativeTracker.actions.Value) {
    var itt=cmb.it.transform
    var firstp=_map(_initSeq.sequence.head).id==cmb.id
    if(itt.isDefinedAt(firstp,action)) {
      action match {
        case InitiativeTracker.actions.Delay => 
          _initSeq.moveDown(cmb.id)
          advanceToNext
          uia ! vcc.view.actor.SetSequence(_initSeq.sequence)
        case InitiativeTracker.actions.ExecuteReady => 
          _initSeq.moveDown(cmb.id)
          uia ! vcc.view.actor.SetSequence(_initSeq.sequence)            
        case InitiativeTracker.actions.EndRound =>
          // When delaying is up, end turn is end of previous
          if(cmb.it.state!=InitiativeState.Delaying) { 
            _initSeq.rotate
            advanceToNext()
          }
          uia ! vcc.view.actor.SetSequence(_initSeq.sequence)
        case InitiativeTracker.actions.Ready => 
          _initSeq.moveDown(cmb.id)
          advanceToNext
          uia ! vcc.view.actor.SetSequence(_initSeq.sequence)            
        case InitiativeTracker.actions.MoveUp => 
          _initSeq.moveUp(cmb.id)
          uia ! vcc.view.actor.SetSequence(_initSeq.sequence)
        case _ =>
      }
      cmb.it=itt(firstp,action)
      uia ! vcc.view.actor.SetInitiative(cmb.id,cmb.it)
    }
  }
}