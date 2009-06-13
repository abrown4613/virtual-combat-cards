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
package vcc.dnd4e.view.dialog

import vcc.dnd4e.model.CombatantType

object EncounterEditorTableEntryProjection extends vcc.util.swing.TableModelRowProjection[EncounterEditorTableEntry] {
  val columns:List[(String,java.lang.Class[_])] = List(
    ("ID",classOf[String]),
    ("Qty",classOf[Integer]),
    ("Name",classOf[String]),
    ("HP",classOf[Integer]),
    ("Init",classOf[Integer]),
    ("AC",classOf[Integer]),
    ("Fort",classOf[Integer]),
    ("Refl",classOf[Integer]),
    ("Will",classOf[Integer])
  )

  val setter:PartialFunction[(Int,EncounterEditorTableEntry,Any),Unit]= {
    case (0,entry,v) if(entry.qty==1)=> 
      entry.id=v.asInstanceOf[String].toUpperCase
    case (1,entry,v) if(entry.id=="" || entry.id == null )=> 
      entry.qty=v.asInstanceOf[Int]
      if(entry.qty<1) entry.qty=1
    case (2,entry,v) => 
      entry.name=v.asInstanceOf[String]
    case (3,entry,v) if(entry.ctype!=CombatantType.Minion)=> 
      entry.hp=v.asInstanceOf[Int]
      if(entry.hp<2) entry.hp=2
    case (4,entry,v)=> entry.init=v.asInstanceOf[Int]
    case (5,entry,v)=> entry.ac=v.asInstanceOf[Int]
    case (6,entry,v)=> entry.fortitude=v.asInstanceOf[Int]
    case (7,entry,v)=> entry.reflex=v.asInstanceOf[Int]
    case (8,entry,v)=> entry.will=v.asInstanceOf[Int]
  }
  
  
  def apply(col:Int,entry:EncounterEditorTableEntry):java.lang.Object = {
    implicit val conv:Int=>java.lang.Object=int2Integer
    col match {
      case 0 => entry.id
      case 1 => entry.qty
      case 2 => entry.name
      case 3 => entry.hp
      case 4 => entry.init
      case 5 => entry.ac
      case 6 => entry.fortitude
      case 7 => entry.reflex
      case 8 => entry.will
    }
  }
}