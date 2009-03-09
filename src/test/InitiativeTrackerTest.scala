//$Id$
package test

import junit.framework.TestCase

import vcc.dnd4e.model._
import vcc.dnd4e.model.InitiativeState._

class InitiativeTrackerTest extends TestCase {

  def testReserve {
    var it=InitiativeTracker(0,Reserve)
    
    assert(!it.canTransform(true,InitiativeTracker.actions.StartRound))
    assert(!it.canTransform(true,InitiativeTracker.actions.EndRound))
    assert(!it.canTransform(true,InitiativeTracker.actions.Delay))
    assert(it.canTransform(true,InitiativeTracker.actions.MoveUp))
    assert(!it.canTransform(true,InitiativeTracker.actions.Ready))
    assert(!it.canTransform(true,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(true,InitiativeTracker.actions.MoveUp)==InitiativeTracker(0,Waiting))
    
    assert(!it.canTransform(false,InitiativeTracker.actions.StartRound))
    assert(!it.canTransform(false,InitiativeTracker.actions.EndRound))
    assert(!it.canTransform(false,InitiativeTracker.actions.Delay))
    assert(it.canTransform(false,InitiativeTracker.actions.MoveUp))
    assert(!it.canTransform(false,InitiativeTracker.actions.Ready))
    assert(!it.canTransform(false,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(false,InitiativeTracker.actions.MoveUp)==InitiativeTracker(0,Waiting))
  }
  

  def testWaiting {
    var it=InitiativeTracker(0,Waiting)
    
    assert(it.canTransform(true,InitiativeTracker.actions.StartRound))
    assert(!it.canTransform(true,InitiativeTracker.actions.EndRound))
    assert(!it.canTransform(true,InitiativeTracker.actions.Delay))
    assert(!it.canTransform(true,InitiativeTracker.actions.MoveUp))
    assert(!it.canTransform(true,InitiativeTracker.actions.Ready))
    assert(!it.canTransform(true,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(true,InitiativeTracker.actions.StartRound)==InitiativeTracker(1,Acting))

    assert(!it.canTransform(false,InitiativeTracker.actions.StartRound))
    assert(!it.canTransform(false,InitiativeTracker.actions.EndRound))
    assert(!it.canTransform(false,InitiativeTracker.actions.Delay))
    assert(!it.canTransform(false,InitiativeTracker.actions.MoveUp))
    assert(!it.canTransform(false,InitiativeTracker.actions.Ready))
    assert(!it.canTransform(false,InitiativeTracker.actions.ExecuteReady))
  
  }  

  def testReady {
    var it=InitiativeTracker(0,Ready)
    
    assert(it.canTransform(true,InitiativeTracker.actions.StartRound))
    assert(!it.canTransform(true,InitiativeTracker.actions.EndRound))
    assert(!it.canTransform(true,InitiativeTracker.actions.Delay))
    assert(!it.canTransform(true,InitiativeTracker.actions.MoveUp))
    assert(!it.canTransform(true,InitiativeTracker.actions.Ready))
    assert(!it.canTransform(true,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(true,InitiativeTracker.actions.StartRound)==InitiativeTracker(1,Acting))
    //assert(it.transform(true,InitiativeTracker.actions.Delay)==InitiativeTracker(1,Delaying))

    assert(!it.canTransform(false,InitiativeTracker.actions.StartRound))
    assert(!it.canTransform(false,InitiativeTracker.actions.EndRound))
    assert(!it.canTransform(false,InitiativeTracker.actions.Delay))
    assert(!it.canTransform(false,InitiativeTracker.actions.MoveUp))
    assert(!it.canTransform(false,InitiativeTracker.actions.Ready))
    assert(it.canTransform(false,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(false,InitiativeTracker.actions.ExecuteReady)==InitiativeTracker(0,Waiting))
  }  

  def testActing {
    var it=InitiativeTracker(0,Acting)
    
    assert(!it.canTransform(true,InitiativeTracker.actions.StartRound))
    assert(it.canTransform(true,InitiativeTracker.actions.EndRound))
    assert(it.canTransform(true,InitiativeTracker.actions.Delay))
    assert(!it.canTransform(true,InitiativeTracker.actions.MoveUp))
    assert(it.canTransform(true,InitiativeTracker.actions.Ready))
    assert(!it.canTransform(true,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(true,InitiativeTracker.actions.EndRound)==InitiativeTracker(0,Waiting))
    assert(it.transform(true,InitiativeTracker.actions.Ready)==InitiativeTracker(0,Ready))
    assert(it.transform(true,InitiativeTracker.actions.Delay)==InitiativeTracker(0,Delaying))
    
    assert(!it.canTransform(false,InitiativeTracker.actions.StartRound))
    assert(!it.canTransform(false,InitiativeTracker.actions.EndRound))
    assert(!it.canTransform(false,InitiativeTracker.actions.Delay))
    assert(!it.canTransform(false,InitiativeTracker.actions.MoveUp))
    assert(!it.canTransform(false,InitiativeTracker.actions.Ready))
    assert(!it.canTransform(false,InitiativeTracker.actions.ExecuteReady))

  }  

  def testDelaying {
    var it=InitiativeTracker(0,Delaying)
    
    assert(!it.canTransform(true,InitiativeTracker.actions.StartRound))
    assert(it.canTransform(true,InitiativeTracker.actions.EndRound))
    assert(!it.canTransform(true,InitiativeTracker.actions.Delay))
    assert(!it.canTransform(true,InitiativeTracker.actions.MoveUp))
    assert(!it.canTransform(true,InitiativeTracker.actions.Ready))
    assert(!it.canTransform(true,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(true,InitiativeTracker.actions.EndRound)==InitiativeTracker(0,Waiting))
    
    assert(!it.canTransform(false,InitiativeTracker.actions.StartRound))
    assert(!it.canTransform(false,InitiativeTracker.actions.EndRound))
    assert(!it.canTransform(false,InitiativeTracker.actions.Delay))
    assert(it.canTransform(false,InitiativeTracker.actions.MoveUp))
    assert(!it.canTransform(false,InitiativeTracker.actions.Ready))
    assert(!it.canTransform(false,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(false,InitiativeTracker.actions.MoveUp)==InitiativeTracker(0,Acting))
  }
  

}
