//$Id$
package vcc.controller

package actions {
  case class LoadPartyFile(file:java.io.File)
  case class LoadPartyFromTemplate(tmpl:Seq[vcc.model.CombatantTemplate])
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
          for(x<-l) coord.tracker ! actions.AddCombatant(x)
          coord.tracker ! actions.Enumerate()          
        case actions.LoadPartyFile(file)=> 
          var l=vcc.model.PartyLoader.loadFromFile(file)
          this ! actions.LoadPartyFromTemplate(l)
      }
    }
  }
}
