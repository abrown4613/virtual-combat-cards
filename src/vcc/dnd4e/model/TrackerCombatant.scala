/**
 * Copyright (C) 2008-2009 tms - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
//$Id$
package vcc.dnd4e.model

import vcc.controller.transaction._

case class CombatantUpdate(comb:Symbol, obj:Any) extends ChangeNotification
case class RosterUpdate(obj:Map[Symbol,TrackerCombatant]) extends ChangeNotification

class TrackerCombatant(val id:Symbol, val alias:String, val name:String,val hp:Int,val init:Int, val ctype:CombatantType.Value, val entity:CombatantEntity ) {
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
