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
package test

import junit.framework.TestCase

import vcc.dnd4e.model._
import vcc.dnd4e.model.InitiativeState._

class InitiativeTrackerTest extends TestCase {

  def runAllCases(it:InitiativeTracker,trans:Map[(Boolean,InitiativeTracker.actions.Value),InitiativeTracker]) {
    for(first<-List(true,false);action<-InitiativeTracker.actions) {
      var pair=(first,action)
      //println("Test "+pair + " transition state to "+trans.getOrElse(pair,"not defined") )
      if(trans.isDefinedAt(pair)) {
    	assert(it.canTransform(first,action), "Transition " + pair + " should be allowed")
    	assert(it.transform(first,action)==trans(pair),"Transition "+ pair + " should take to "+trans(pair) + " but I got "+it.transform(first,action))
      } else {
    	assert(!it.canTransform(first,action), "Transition " + pair + " should not be allowed")
      }
    }
  }
  
  def testReserve {
    var it=InitiativeTracker(0,Reserve)
    
    runAllCases(it,Map(
      (true,InitiativeTracker.actions.MoveUp)->InitiativeTracker(0,Waiting),
      (false,InitiativeTracker.actions.MoveUp)->InitiativeTracker(0,Waiting)
    ))
  }
  

  def testWaiting {
    var it=InitiativeTracker(0,Waiting)
    
    runAllCases(it,Map(
      (true,InitiativeTracker.actions.StartRound)->InitiativeTracker(1,Acting)
    ))
  }  

  def testReadying {
    var it=InitiativeTracker(0,Readying)
    
    runAllCases(it,Map(
      (true,InitiativeTracker.actions.EndRound)->InitiativeTracker(0,Ready)
    ))
  }
  
  def testReady {
    var it=InitiativeTracker(0,Ready)
    
    runAllCases(it,Map(
      (true,InitiativeTracker.actions.StartRound)->InitiativeTracker(1,Acting),
      (false,InitiativeTracker.actions.ExecuteReady)->InitiativeTracker(0,Waiting)
    ))
  }  

  def testActing {
    var it=InitiativeTracker(0,Acting)
    
    runAllCases(it,Map(
      (true,InitiativeTracker.actions.EndRound)->InitiativeTracker(0,Waiting),
      (true,InitiativeTracker.actions.Ready)->InitiativeTracker(0,Readying),
      (true,InitiativeTracker.actions.Delay)->InitiativeTracker(0,Delaying)
    ))
  }  

  def testDelaying {
    var it=InitiativeTracker(0,Delaying)
    runAllCases(it,Map(
      (true,InitiativeTracker.actions.EndRound)->InitiativeTracker(0,Waiting),
      (false,InitiativeTracker.actions.MoveUp)->InitiativeTracker(0,Acting)
    ))
  }
}
