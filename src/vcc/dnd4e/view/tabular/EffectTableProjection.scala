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
package vcc.dnd4e.view.tabular

import vcc.util.swing.TableModelRowProjection
import vcc.dnd4e.model.{Effect,Condition}

class EffectTableProjection(tracker:scala.actors.Actor) extends TableModelRowProjection[(Symbol,Int,Effect)]{
  
  val columns:List[(String,java.lang.Class[_])] = List(
    ("Src",classOf[String]),
    ("End",classOf[String]),
    ("Description",classOf[String]))
  
  def apply(col:Int,entry:(Symbol,Int,Effect)):java.lang.Object = {
    col match {
      case 0 => entry._3.source.name
      case 1 => entry._3.duration.shortDesc
      case 2 => entry._3.condition.description
    }
  }
  
  val setter:PartialFunction[(Int,(Symbol,Int,Effect),Any),Unit]= {
    case (2,(who,pos,Effect(_,Condition.Generic(x),_,_)),newvalue) =>
      tracker ! vcc.dnd4e.controller.request.UpdateEffect(who,pos,Condition.Generic(newvalue.asInstanceOf[String]))
  }
}
