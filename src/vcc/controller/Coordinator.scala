//$Id$
package vcc.controller

import scala.actors.Actor

package actions {
  case class SetCoordinator(coord:Coordinator)
  case class AddObserver(obs:Actor)
}

object Coordinator {

  def initialize[C](tc:TrackerController[C]):Coordinator = {
    val tracker=new Tracker(tc)
    val loader=new EntityLoader();
    
    new Coordinator(tracker,loader)
  }
}

class Coordinator(val tracker:Actor, val loader:Actor) {
  def start() {
    tracker.start
    loader.start
    tracker ! actions.SetCoordinator(this)
    loader ! actions.SetCoordinator(this)
  }
  
  def addObserver(obs:Actor) {
    tracker ! actions.AddObserver(obs)
  }
}
