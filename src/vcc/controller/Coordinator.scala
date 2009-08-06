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

import scala.actors.Actor

import vcc.model.Registry

package actions {
  case class SetCoordinator(coord:Coordinator)
  case class AddObserver(obs:Actor)
}

//FIXME: destroy this object
object Coordinator {

  def initialize[C](tc:TrackerController[C]):Coordinator = {
    val tracker=new Tracker(tc)
    
    Registry.register[Actor]("tracker",tracker)
    
    new Coordinator(tracker)
  }
}

class Coordinator(val tracker:Actor) {
  def start() {
    tracker.start
    tracker ! actions.SetCoordinator(this)
  }
  
  def addObserver(obs:Actor) {
    tracker ! actions.AddObserver(obs)
  }
}
