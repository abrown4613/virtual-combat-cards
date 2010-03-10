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

import vcc.model.{IDGenerator,CombatSequencer}

import vcc.controller.transaction._

class TrackerContext {
  val idgen= new IDGenerator(1,50)
  
  val sequence=new CombatSequencer[SequenceChange]( x => SequenceChange(x.toSeq) )
  
  private var _undo_map=new Undoable[Map[Symbol,TrackerCombatant]](
    Map.empty[Symbol,TrackerCombatant],
    un=>RosterUpdate(Map(un.value.map(me =>(me._1 ->me._2.toState())).toSeq:_*))
  )
  
  def map_=(m:Map[Symbol,TrackerCombatant])(implicit trans:Transaction)= _undo_map.value=m
  def map = _undo_map.value

  object InMap {
    def unapply(id:Symbol):Option[TrackerCombatant]= if(map.contains(id)) Some(map(id)) else None
  }
  
  def allCombatant:Seq[TrackerCombatant] = _undo_map.value.map(entry=>entry._2).toSeq
}
