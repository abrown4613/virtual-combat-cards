package test

import junit.framework.TestCase

import vcc.model._
import vcc.model.InitiativeState._

class InitiativeTrackerTest extends TestCase {

  def testReserve {
    var it=InitiativeTracker(0,Reserve)
    
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.StartRound))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.EndRound))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.Delay))
    assert(it.transform.isDefinedAt(true,InitiativeTracker.actions.MoveUp))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.Ready))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(true,InitiativeTracker.actions.MoveUp)==InitiativeTracker(01,Acting))
    
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.StartRound))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.EndRound))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.Delay))
    assert(it.transform.isDefinedAt(false,InitiativeTracker.actions.MoveUp))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.Ready))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(false,InitiativeTracker.actions.MoveUp)==InitiativeTracker(1,Acting))
  }
  

  def testWaiting {
    var it=InitiativeTracker(0,Waiting)
    
    assert(it.transform.isDefinedAt(true,InitiativeTracker.actions.StartRound))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.EndRound))
    assert(it.transform.isDefinedAt(true,InitiativeTracker.actions.Delay))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.MoveUp))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.Ready))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(true,InitiativeTracker.actions.StartRound)==InitiativeTracker(1,Acting))
    assert(it.transform(true,InitiativeTracker.actions.Delay)==InitiativeTracker(0,Delaying))

    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.StartRound))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.EndRound))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.Delay))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.MoveUp))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.Ready))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.ExecuteReady))
  
  }  

  def testReady {
    var it=InitiativeTracker(0,Ready)
    
    assert(it.transform.isDefinedAt(true,InitiativeTracker.actions.StartRound))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.EndRound))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.Delay))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.MoveUp))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.Ready))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(true,InitiativeTracker.actions.StartRound)==InitiativeTracker(1,Acting))

    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.StartRound))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.EndRound))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.Delay))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.MoveUp))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.Ready))
    assert(it.transform.isDefinedAt(false,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(false,InitiativeTracker.actions.ExecuteReady)==InitiativeTracker(0,Waiting))
  }  

  def testActing {
    var it=InitiativeTracker(0,Acting)
    
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.StartRound))
    assert(it.transform.isDefinedAt(true,InitiativeTracker.actions.EndRound))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.Delay))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.MoveUp))
    assert(it.transform.isDefinedAt(true,InitiativeTracker.actions.Ready))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(true,InitiativeTracker.actions.EndRound)==InitiativeTracker(0,Waiting))
    assert(it.transform(true,InitiativeTracker.actions.Ready)==InitiativeTracker(0,Ready))
    
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.StartRound))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.EndRound))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.Delay))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.MoveUp))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.Ready))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.ExecuteReady))

  }  

  def testDelaying {
    var it=InitiativeTracker(0,Delaying)
    
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.StartRound))
    assert(it.transform.isDefinedAt(true,InitiativeTracker.actions.EndRound))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.Delay))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.MoveUp))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.Ready))
    assert(!it.transform.isDefinedAt(true,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(true,InitiativeTracker.actions.EndRound)==InitiativeTracker(1,Waiting))
    
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.StartRound))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.EndRound))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.Delay))
    assert(it.transform.isDefinedAt(false,InitiativeTracker.actions.MoveUp))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.Ready))
    assert(!it.transform.isDefinedAt(false,InitiativeTracker.actions.ExecuteReady))
    assert(it.transform(false,InitiativeTracker.actions.MoveUp)==InitiativeTracker(1,Acting))
  }
  

}
