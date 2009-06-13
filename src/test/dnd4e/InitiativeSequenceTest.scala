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
package test.dnd4e

import junit.framework.TestCase
import vcc.controller.TransactionalProcessor
import vcc.controller.TrackerController
import vcc.controller.transaction.Transaction
import vcc.controller.actions.TransactionalAction
import vcc.dnd4e.model._
import InitiativeState._
import vcc.dnd4e.controller._

//TODO: Must deprecate
import  vcc.dnd4e.view.actor._ 

import test.helper._

trait ActionRecorder {
  def actionsExecuted:List[TransactionalAction]
}

trait ActionAccumulator extends TransactionalProcessor[TrackerContext]{
  self: TransactionalProcessor[TrackerContext] =>
  
  var actionsProcessed:List[TransactionalAction]=Nil
  
  override def rewriteEnqueue(action:TransactionalAction) {
    actionsProcessed=Nil
    super.rewriteEnqueue(action)
  }
  
  addHandler {
    case msg =>
      actionsProcessed= actionsProcessed ::: List(msg)
  }
}


class InitiativeSequenceTest extends TestCase {
  var mockTracker:TrackerMockup[TrackerContext] with ActionRecorder =null
  val seqMatcher = new Matcher[Seq[Symbol]]({
  	case SetSequence(seq) =>seq
  })

  val initMatcher = new Matcher[(Symbol,InitiativeTracker)]({
  	case SetInitiative(comb,s) =>(comb,s)
  })
  
  override def setUp() {
    val context=new TrackerContext()
    val loadHandler=new TransactionalProcessor(context) with TrackerContextHandler
    val trans1=new Transaction()
    val trans1pub=new SetChangePublisher()
    assert(true)
    
    for(cid<-List('A,'B,'C,'D,'E)) {
    	loadHandler.dispatch(trans1,request.AddCombatant(new CombatantTemplate("Comb"+cid.name,10,5,CombatantType.Monster){id=cid.name}))
    }
    trans1.commit(trans1pub)
    
    //Setup the test subjects
    mockTracker=new TrackerMockup(new TrackerController(context) with ActionRecorder  {
      //addHandler(new TrackerEffectHandler(context))
      val processor= new TransactionalProcessor[TrackerContext](context) with InitiativeActionHandler with ActionAccumulator

      addPublisher(new InitiativeChangePublisher(context))
      
      def actionsExecuted:List[TransactionalAction]= processor.actionsProcessed
    }) with ActionRecorder {
      def actionsExecuted:List[TransactionalAction] = this.controller.asInstanceOf[ActionRecorder].actionsExecuted
    }
  }
  
  def testStartCombat() {
	mockTracker.dispatch(request.StartCombat(Seq('A,'B,'C,'D)))
	val s1 = extractCombatSequence()
	val ai1= extractCombatantInitiatives()
	assert(s1==List('A,'B,'C,'D,'E),s1)
	val binit=InitiativeTracker(0,InitiativeState.Waiting)
	assert(ai1.contains('A,binit))
	assert(ai1.contains('B,binit))
	assert(ai1.contains('C,binit))
	assert(ai1.contains('D,binit))
  }
  
  def testEndCombat() {
    testStartCombat()
    mockTracker.dispatch(request.EndCombat())
	val ai1= extractCombatantInitiatives()
	val binit=InitiativeTracker(0,InitiativeState.Reserve)
	assert(ai1.contains('A,binit))
	assert(ai1.contains('B,binit))
	assert(ai1.contains('C,binit))
	assert(ai1.contains('D,binit))
    
  }
  
  /**
   * Delay combatant, move some rounds up, then jump guy in front
   */
  def testDelay() {
	testStartCombat()
	mockTracker.dispatch(request.Delay('A))
	val ci1=extractCombatantInitiatives()
	assert(ci1.contains('A,InitiativeTracker(1,Delaying)))
	val s1=extractCombatSequence()
	assert(s1==List('B,'C,'D,'A,'E))
	
	startRound('B)
	endRound('B,List('C,'D,'A,'B,'E))
 
	mockTracker.dispatch(request.MoveUp('A))
	val ci4=extractCombatantInitiatives()
	assert(ci4.contains('A,InitiativeTracker(1,Acting)))
	val s4=extractCombatSequence()
	assert(s4==List('A,'C,'D,'B,'E))
 
	endRound('A,List('C,'D,'B,'A,'E))

	mockTracker.dispatch(request.Delay('C))
	val ci2=extractCombatantInitiatives()
	assert(ci2.contains('C,InitiativeTracker(1,Delaying)))
	assert(extractCombatSequence()==List('D,'B,'A,'C,'E))
 
	startRound('D)
	endRound('D,List('B,'A,'C,'D,'E))

	startRound('B)
	endRound('B,List('A,'C,'D,'B,'E))

	startRound('A)
	endRound('A,List('C,'D,'B,'A,'E))
 
	// The delaying guy C has to end last round, so sequence does not change
    mockTracker.dispatch(request.EndRound('C))
	sequenceUnchanged()
	assert(extractCombatantInitiatives().contains('C,InitiativeTracker(1,Waiting)),extractCombatantInitiatives())
  }
  
  def testReadyAction() {
	testStartCombat()

	startRound('A)
 
	mockTracker.dispatch(request.Ready('A))
	val ci2=extractCombatantInitiatives()
	assert(ci2.contains('A,InitiativeTracker(1,Ready)))
	assert(extractCombatSequence()==List('B,'C,'D,'A,'E))
 
 	// Make sure that actions are called internally: In our case end of turn
	val al=for(request.InternalInitiativeAction(cmb,act)<-mockTracker.actionsExecuted) yield (cmb.id,act)
	assert(al==List(('A,InitiativeTracker.actions.Ready),('A,InitiativeTracker.actions.EndRound)))

 
	startRound('B)
 	endRound('B,List('C,'D,'A,'B,'E))

 	startRound('C)
 	mockTracker.dispatch(request.ExecuteReady('A))
	val ci3=extractCombatantInitiatives()
	assert(ci3==List(('A,InitiativeTracker(1,Waiting))),ci3)
	assert(extractCombatSequence()==List('C,'D,'B,'A,'E))
  }
  
  def killSomeCombatant(seq:Seq[Symbol]) {
	implicit val trans=new Transaction()
	for(x<-seq) {
	  val cmb=mockTracker.controller.context.map(x)
	  cmb.health=cmb.health.applyDamage(20)
    }
    trans.commit(null)
  }
  
  /**
   * Start and end round in front of a bunch of dead guys, must skip
   */
  def testAutoStartDead() {
    import InitiativeTracker.actions._
    testStartCombat()
    killSomeCombatant(List('B,'C))

    mockTracker.dispatch(request.StartRound('A))
	val ci1=extractCombatantInitiatives()
	assert(ci1.contains('A,InitiativeTracker(1,Acting)))
	sequenceUnchanged()

	mockTracker.dispatch(request.EndRound('A))
	val ci3=extractCombatantInitiatives()
	assert(ci3.contains('A,InitiativeTracker(1,Waiting)))
	assert(ci3.contains('B,InitiativeTracker(1,Waiting)))
	assert(ci3.contains('C,InitiativeTracker(1,Waiting)))
	assert(extractCombatSequence()==List('D,'A,'B,'C,'E))
    
	// Make sure that actions are called internally
	val al=for(request.InternalInitiativeAction(cmb,act)<-mockTracker.actionsExecuted) yield (cmb.id,act)
	assert(al==List(('A,EndRound),('B,StartRound),('B,EndRound),('C,StartRound),('C,EndRound)))
  }
  
  /**
   * Start and end round in front of a bunch of dead guys, must skip
   */
  def testAutoStartDeadOnDelay() {
    import InitiativeTracker.actions._
    testStartCombat()
    killSomeCombatant(List('B,'C))

    mockTracker.dispatch(request.Delay('A))
	val ci1=extractCombatantInitiatives()
	assert(ci1.contains('A,InitiativeTracker(1,Delaying)))
	assert(ci1.contains('B,InitiativeTracker(1,Waiting)))
	assert(ci1.contains('C,InitiativeTracker(1,Waiting)))
	assert(extractCombatSequence()==List('D,'A,'B,'C,'E))
    
	// Make sure that actions are called internally
	val al=for(request.InternalInitiativeAction(cmb,act)<-mockTracker.actionsExecuted) yield (cmb.id,act)
	assert(al==List(('A,StartRound),('A,Delay),('B,StartRound),('B,EndRound),('C,StartRound),('C,EndRound)))
  }
  
  /**
   * Start and end round in front of a bunch of dead guys, must skip
   */
  def testAutoStartDeadOnReady() {
    import InitiativeTracker.actions._
    testStartCombat()
    killSomeCombatant(List('B,'C))

    mockTracker.dispatch(request.StartRound('A))
	val ci1=extractCombatantInitiatives()
	assert(ci1.contains('A,InitiativeTracker(1,Acting)))
	sequenceUnchanged()

    mockTracker.dispatch(request.Ready('A))
	val ci2=extractCombatantInitiatives()
	assert(ci2.contains(('A,InitiativeTracker(1,InitiativeState.Ready))))
	assert(ci2.contains(('B,InitiativeTracker(1,Waiting))))
	assert(ci2.contains(('C,InitiativeTracker(1,Waiting))))
	assert(extractCombatSequence()==List('D,'A,'B,'C,'E))
    
	// Make sure that actions are called internally
	val al=for(request.InternalInitiativeAction(cmb,act)<-mockTracker.actionsExecuted) yield (cmb.id,act)
	assert(al==List(('A,Ready),('A,EndRound),('B,StartRound),('B,EndRound),('C,StartRound),('C,EndRound)))
  }

  /**
   * Start and end round in front of a bunch of dead guys, must skip the dead
   * including Delaying deads
   */
  def testAutoStartDelayedDeadIssue85() {
    import InitiativeTracker.actions._
    testStartCombat()
    

    startRound('A)
    mockTracker.dispatch(request.Ready('A))
	assert(extractCombatantInitiatives().contains('A,InitiativeTracker(1,InitiativeState.Ready)))

	mockTracker.dispatch(request.Delay('B))
	assert(extractCombatantInitiatives().contains('B,InitiativeTracker(1,InitiativeState.Delaying)))

	assert(extractCombatSequence()==List('C,'D,'A,'B,'E))

	killSomeCombatant(List('A,'B))
 
	startRound('C)
	endRound('C,List('D,'A,'B,'C,'E))

	startRound('D)
	endRound('D,List('C,'D,'A,'B,'E))
    
	// Make sure that actions are called internally
	val al=for(request.InternalInitiativeAction(cmb,act)<-mockTracker.actionsExecuted) yield (cmb.id,act)
	assert(al==List(('D,EndRound),('A,StartRound),('A,EndRound),('B,EndRound),('B,StartRound),('B,EndRound)),al)
  }
  
  def testMoveOutOfReserve() {
    testStartCombat()
    mockTracker.dispatch(request.MoveUp('E))
	assert(extractCombatSequence()==List('E,'A,'B,'C,'D))
	assert(extractCombatantInitiatives().contains('E,InitiativeTracker(1,Acting)))
  }
  
  
  /**
   * Test move before action, this is an initiative sequence actions so it should:
   * 1) Not change tracker status
   * 2) Allow move before acting combatant
   * 3) Allow move before reserver
   * 4) Move acting char
   */
  def testMoveBefore() {
    testStartCombat()
	val s1 = extractCombatSequence()
	val ai1= extractCombatantInitiatives()
	assert(s1==List('A,'B,'C,'D,'E),s1)

	startRound('A)

	// Move A before D, this must fail because A is acting
	mockTracker.dispatch(request.MoveBefore('A,'D))
	sequenceUnchanged()
 
	// Move D befora A, must fail since A is acting
	mockTracker.dispatch(request.MoveBefore('D,'A))
	sequenceUnchanged()

	// Move F befora D, must fail because 'F is not in the sequence
	mockTracker.dispatch(request.MoveBefore('D,'A))
	sequenceUnchanged()

 	// Move E befora B, must fail since E is in reserve
	mockTracker.dispatch(request.MoveBefore('E,'B))
	sequenceUnchanged()

  	// Move B befora B, must fail since move the same
	mockTracker.dispatch(request.MoveBefore('B,'B))
	sequenceUnchanged()

	// Move D befora B, should work
	mockTracker.dispatch(request.MoveBefore('D,'B))
	val ci2=extractCombatantInitiatives()
	assert(ci2==Nil,ci2)
	assert(extractCombatSequence()==List('A,'D,'B,'C,'E),extractCombatSequence)

	// Move C befora E, must fail since E is in reserve
	mockTracker.dispatch(request.MoveBefore('C,'E))
	sequenceUnchanged()

	endRound('A,List('D,'B,'C,'A,'E))

	// Move D before B (move back), should work 
	mockTracker.dispatch(request.MoveBefore('D, 'C))
	val ci3=extractCombatantInitiatives()
	assert(ci3==Nil,ci3)
	assert(extractCombatSequence()==List('B,'D,'C,'A,'E))
 
  }

  def startRound(who:Symbol) {
	val init=mockTracker.controller.context.map(who).it.value
	mockTracker.dispatch(request.StartRound(who))
	val ci=extractCombatantInitiatives()
	assert(ci.contains(who,init.transform(true,InitiativeTracker.actions.StartRound)))
	sequenceUnchanged()
  } 
  
  def endRound(who:Symbol,nseq:Seq[Symbol]) {
	val init=mockTracker.controller.context.map(who).it.value
	mockTracker.dispatch(request.EndRound(who))
	val ci=extractCombatantInitiatives()
	assert(ci.contains(who,init.transform(true,InitiativeTracker.actions.EndRound)))
	val s2=extractCombatSequence()
	assert(s2==nseq, "Sequence is not correct got "+s2+ " expected "+nseq )
  
  } 
  
  def sequenceUnchanged() {
    mockTracker.lastChangeMessages match {
      case seqMatcher.findAll() => assert(true)
      case seqMatcher.findAll(x @ _*) => assert(false, "should not have changed sequence")
    }
  }
  
  def extractCombatSequence() = {
    mockTracker.lastChangeMessages match {
      case seqMatcher.findAll(iseq) => iseq
      case _ => 
        assert(false,"Cant get new sequence list for matcher "+seqMatcher)
        Nil
    }
  }
  
  def extractCombatantInitiatives():Seq[(Symbol,InitiativeTracker)] = {
    mockTracker.lastChangeMessages match {
      case initMatcher.findAll(iseq @ _*) => iseq
    }
  }
}
