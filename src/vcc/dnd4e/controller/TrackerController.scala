//$Id$
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
package vcc.dnd4e.controller

import vcc.controller.{TrackerController=>AbstractTrackerController}
import vcc.controller.TransactionalProcessor
import vcc.controller.transaction.ChangeNotification
import vcc.dnd4e.controller._
import vcc.dnd4e.model.TrackerContext
import vcc.dnd4e.model.{CombatStateChanged,CombatStateChange}

/**
 * This Mixin implement the <code>publish</code> method required for a TrackerController, it will
 * simply cast all objects to the CombatStateChange and make sure every changes was accounted for.
 */
trait TrackerControllerValidatingPublisher[C] extends AbstractTrackerController[C] {
  
  def publish(changes:Seq[ChangeNotification]):Any = {
    val c:Seq[CombatStateChange] = for(change <- changes if(change.isInstanceOf[CombatStateChange])) yield { 
      change.asInstanceOf[CombatStateChange]
    }
    assert(c.length == changes.length)
    CombatStateChanged(c.toList.toSeq)
  }
    
}

/**
 * This is the DND4E TrackerController with all need traits and implementation.
 */
class TrackerController() extends AbstractTrackerController[TrackerContext](new TrackerContext()) 
  with TrackerControllerValidatingPublisher[TrackerContext] 
{
    val processor= new TransactionalProcessor[TrackerContext](context) with TrackerEffectHandler with TrackerContextHandler with InitiativeActionHandler
  
}
