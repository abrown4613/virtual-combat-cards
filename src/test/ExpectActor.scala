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
package test

import scala.actors.{Actor,TIMEOUT}

/**
 * Behavior is what the actor should to. The partial function is defined to process 
 * messages recieved by the ExpectActor. It should return None if there's nothing to 
 * say or Some(x) if there is feedback. Sending feedback will terminate the ExpectActor
 */
abstract class Behavior {
  def running():Boolean
  val behavior:PartialFunction[Any,Option[Any]]
  def run(a:Any):Option[Any] = if(behavior.isDefinedAt(a)) behavior(a) else None
}

class ExpectingBehavior[T](var expecting:List[T]) extends Behavior {
  def running=expecting!=Nil
  val behavior:PartialFunction[Any,Option[Any]]={
    case s => 
      if(s == expecting.head) { expecting=expecting.tail; None}
      else { expecting=Nil; Some(s); }
  }
}

class AccumulatingBehavior extends Behavior {
  var _acc:List[Any]=Nil
  def running():Boolean=true
  val behavior:PartialFunction[Any,Option[Any]] = {
    case s => _acc= s :: _acc; None
  }
  def accumulated()=_acc.reverse
}


class ExpectActor(timeout:Long,var behavior:Behavior,peer:Actor) extends Actor {
  // These are the expected input and output
  case class Timeout(who:Actor)
  case class Feedback(who:Actor,what:Any)
  case class ChangeBehavior(b:Behavior)
  case class Done(who:Actor)
  
  def act() = {
    while(behavior.running) {
      receiveWithin(timeout) {
        case ChangeBehavior(b) => behavior=b
        case TIMEOUT => { peer ! Timeout(this) ; exit}
        case s => var opt=behavior.run(s)
        if(opt.isDefined) { peer ! Feedback(this,opt.get); exit } 
      }
    }
    peer ! Done(this)
    exit
  }
}
