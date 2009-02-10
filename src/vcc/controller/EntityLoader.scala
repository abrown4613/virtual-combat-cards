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
          for(x<-l) coord.tracker ! vcc.dnd4e.controller.actions.AddCombatant(x)
          //coord.tracker ! actions.Enumerate()
          coord.tracker ! actions.EndTransaction("Party Load")
        case actions.LoadPartyFile(file)=> 
          var l=PartyLoader.loadFromFile(file)
          this ! actions.LoadPartyFromTemplate(l)
      }
    }
  }
}
