//$Id$
package vcc.dnd4e.model

import vcc.model.{IDGenerator,CombatSequencer}

import vcc.controller.transaction._

class TrackerContext {
  val idgen= new IDGenerator(1,50)
  
  val sequence=new CombatSequencer
  
  private var _undo_map=new Undoable[Map[Symbol,TrackerCombatant]](Map.empty[Symbol,TrackerCombatant],un=>RosterUpdate(un.value))
  
  def map_=(m:Map[Symbol,TrackerCombatant])(implicit trans:Transaction)= _undo_map.value=m
  def map = _undo_map.value

  object InMap {
    def unapply(id:Symbol):Option[TrackerCombatant]= if(map.contains(id)) Some(map(id)) else None
  }
}
