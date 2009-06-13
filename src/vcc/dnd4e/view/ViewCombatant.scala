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
package vcc.dnd4e.view 

import vcc.dnd4e.model._

trait UIFormatted {
  def formattedText():String
}	

case class ViewCombatant(id:Symbol,name:String,hp:Int,init:Int,defense:DefenseBlock) {
  var health=HealthTracker.createTracker(CharacterHealthDefinition(hp,hp/4,4))
  var initTracker=InitiativeTracker(0,InitiativeState.Reserve)
  var info:String=""
  var effects:List[Effect]=Nil
  
  def isCharacter:Boolean = health.base.ctype==CombatantType.Character
}

object ViewCombatantProjection extends vcc.util.swing.TableModelRowProjection[ViewCombatant] {
  override val columns=List[(String,java.lang.Class[_])](
    ("ID",classOf[java.lang.String]),
    ("Name",classOf[String]),
    ("Health",classOf[String]),
    ("Status",classOf[String]),
    ("Turn #",classOf[Integer]),
    ("Sequence",classOf[String])
  );
  def apply(col:Int,comb:ViewCombatant):java.lang.Object= {
    col match {
      case 0 => comb.id.name
      case 1 => comb.name
      case 2 => comb.health.currentHP + " / "+comb.hp + (if(comb.health.temporaryHP>0) " +"+comb.health.temporaryHP else "")
      case 3 => comb.health.status + (if(comb.health.status==HealthTracker.Status.Dying) ("(" + comb.health.deathStrikes + "/3)") else "!!!".substring(0,comb.health.deathStrikes)) 
      case 4 => int2Integer(comb.initTracker.round)
      case 5 => comb.initTracker.state
    }
  }
  val setter = null
}
