//$Id$
package vcc.dnd4e.model

import vcc.controller.transaction._

case class CombatantUpdate(comb:Symbol, obj:Any) extends ChangeNotification
case class RosterUpdate(obj:Map[Symbol,TrackerCombatant]) extends ChangeNotification

class TrackerCombatant(val id:Symbol,val name:String,val hp:Int,val init:Int, val ctype:CombatantType.Value) {
  private var _health=new Undoable[HealthTracker](HealthTracker.createTracker(ctype,hp),(uv)=>CombatantUpdate(id,uv.value))
  
  def health= _health.value
  def health_=(nh:HealthTracker)(implicit trans:Transaction) {
    if(nh != _health.value) _health.value=nh
    this
  }
  
  private val _info= new Undoable[String]("",uv=>{CombatantUpdate(id,uv.value)})
  def info= _info.value
  def info_=(str:String)(implicit trans:Transaction) { _info.value=str; this }
  
  var it=new Undoable[InitiativeTracker](InitiativeTracker(0,InitiativeState.Reserve),(uv)=>{CombatantUpdate(id,uv.value)})
  var defense:DefenseBlock=null
  
  
  private var _effects=new Undoable[EffectList](EffectList(Nil),uv=>{CombatantUpdate(id,uv.value)})
  
  /**
   * Return the lists of active effect on the list.
   */
  def effects:EffectList=_effects.value
  
  def effects_=(nel:EffectList)(implicit trans:Transaction) {
    if(effects != nel)
      _effects.value=nel; 
    this 
  }
  
}
