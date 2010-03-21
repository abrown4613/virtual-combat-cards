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
import scala.actors.Actor.loop

import vcc.controller.transaction._
import vcc.model.Registry
import vcc.infra.startup.StartupStep
import vcc.controller.message.TransactionalAction

/**
 * Tracker actor handles the core logic for the event dispatch loop. It controls
 * transactions (start, end and clearing the log), undo/redo, and observer registration.
 * It will dispatch actions and query to the controller handlers, and will gather return
 * data to be passed on to the observer.
 * @param controller Action and query logic controller
 */
class Tracker(controller:TrackerController[_]) extends Actor with StartupStep with TransactionChangePublisher {
  
  private val logger = org.slf4j.LoggerFactory.getLogger("user")
  def isStartupComplete = true 
  
  private var observers:List[Actor]=Nil
  
  private val _tlog= new TransactionLog[TransactionalAction]()  
  
  /**
   * Publish changes to the observers
   */
  def publishChange(changes:Seq[ChangeNotification]) {
    val msg = controller.publish(changes)
    for(obs<-observers) obs ! msg
  }
  
  def act()={
    loop {
      react {
        case message.AddObserver(obs) => 
          observers=obs::observers
          
        case message.Command(from,action) => 
          val trans= new Transaction()
          try {
        	controller.dispatch(trans,from,action)
        	trans.commit(this) 
        	if(!trans.isEmpty) {
        		_tlog.store(action,trans)
        		logger.info("TLOG["+ _tlog.length +"] Added transaction: "+ _tlog.pastActions.head.description)
        	}
        	from.actionCompleted(action.description)
          } catch {
            case e => 
              logger.warn("An exception occured while processing: "+ action,e)
              e.printStackTrace(System.out)
              logger.warn("Rolling back transaction")
              if(trans.state == Transaction.state.Active) trans.cancel()
              from.actionCancelled(e.getMessage)
          }
        case message.Undo() =>
          try {
            _tlog.rollback(this)
          } catch { case s:TransactionLogOutOfBounds => }
        case message.Redo() =>
          try {
            _tlog.rollforward(this)
          } catch { case s:TransactionLogOutOfBounds => }
        case message.ClearTransactionLog() =>
          _tlog.clear
          
        case s=>
          logger.warn("Error: Tracker can't handle this event: "+s)
      }
    }
  }
}

object Tracker {
  def initialize(tc:TrackerController[_]):Tracker = {
    val tracker=new Tracker(tc)
    Registry.register[Actor]("tracker",tracker)
    tracker.start
    tracker
  }
  
}