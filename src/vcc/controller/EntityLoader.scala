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
package vcc.controller

import vcc.dnd4e.model.{CombatantTemplate,PartyLoader}
import vcc.dnd4e.controller._

package actions {
  case class LoadPartyFile(file:java.io.File)
  case class LoadPartyFromTemplate(tmpl:Seq[CombatantTemplate])
}

import scala.actors.Actor
import scala.actors.Actor.{loop,react}

class EntityLoader extends Actor {

  private var coord:Coordinator=null
  
  def act() {
    loop {
      react {
        case actions.SetCoordinator(coord) => this.coord=coord
        case actions.LoadPartyFromTemplate(l)=>
          coord.tracker ! actions.StartTransaction("Party Load")
          for(x<-l) coord.tracker ! vcc.dnd4e.controller.request.AddCombatant(x)
          //coord.tracker ! actions.Enumerate()
          coord.tracker ! actions.EndTransaction("Party Load")
        case actions.LoadPartyFile(file)=> 
          var l=PartyLoader.loadFromFile(file)
          this ! actions.LoadPartyFromTemplate(l)
      }
    }
  }
}
